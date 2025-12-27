let packageData = {}; // global so other handlers can use it
let pkgDescCache = {}; // cache package -> description so node clicks don't spam backend

// state so we can re-apply the right view
let currentSelectedNodeId = null;
let currentNodeChainVisited = new Set();
let currentNodeChainEdges = new Set();

let currentFunctionDepPkgs = new Set(); // names returned by /analyze (plus root pkg)

// ------------------------
// sidebar + panels
// ------------------------

function ensureLeftSidebar() {
  let sidebar = document.getElementById("left-sidebar");
  if (sidebar) return sidebar;

  sidebar = document.createElement("div");
  sidebar.id = "left-sidebar";
  sidebar.style.position = "absolute";
  sidebar.style.top = "10%";
  sidebar.style.left = "15px";
  sidebar.style.width = "20%";
  sidebar.style.maxHeight = "80%";
  sidebar.style.display = "flex";
  sidebar.style.flexDirection = "column";
  sidebar.style.gap = "12px";
  sidebar.style.zIndex = 9999;

  document.body.appendChild(sidebar);
  return sidebar;
}

function ensurePackagePanel() {
  let panel = document.getElementById("pkg-panel");
  if (panel) return panel;

  const sidebar = ensureLeftSidebar();

  panel = document.createElement("div");
  panel.id = "pkg-panel";
  panel.style.background = "rgb(249, 249, 249)";
  panel.style.border = "1px solid rgb(204, 204, 204)";
  panel.style.overflowY = "auto";
  panel.style.fontFamily = "Arial, sans-serif";
  panel.style.boxShadow = "rgba(0, 0, 0, 0.2) 0px 2px 4px";
  panel.style.borderRadius = "8px";
  panel.style.display = "none";
  panel.style.maxHeight = "32%";

  panel.innerHTML = `
    <div style="position:sticky;top:0;background:#f9f9f9;z-index:2;padding: 14px 16px;border-bottom:1px solid #ddd;">
      <h2 style="margin:0;">Package Info</h2>
      <div id="pkg-panel-name" style="color: red; margin-top:6px;font-weight:bold;font-size:16px;"></div>
    </div>
    <div id="pkg-panel-desc" style="color:#333; padding: 12px 16px; font-size: 14px; line-height: 1.35;">
      <i>Select a package to see its description.</i>
    </div>
  `;

  sidebar.appendChild(panel);
  return panel;
}

function mountDepPanelIntoSidebar() {
  const sidebar = ensureLeftSidebar();
  const depPanel = document.getElementById("dep-panel");
  if (!depPanel) return null;

  depPanel.style.position = "relative";
  depPanel.style.top = "";
  depPanel.style.left = "";
  depPanel.style.right = "";
  depPanel.style.marginLeft = "0";
  depPanel.style.width = "100%";
  depPanel.style.height = "auto";
  depPanel.style.maxHeight = "48%";

  if (depPanel.parentElement !== sidebar) {
    sidebar.appendChild(depPanel);
  }

  // hide old top close
  const closeTop = document.getElementById("close-dep");
  if (closeTop) closeTop.style.display = "none";

  // sticky footer close button (only once)
  if (!document.getElementById("dep-footer")) {
    const footer = document.createElement("div");
    footer.id = "dep-footer";
    footer.style.position = "sticky";
    footer.style.bottom = "0";
    footer.style.background = "#f9f9f9";
    footer.style.padding = "10px 16px";
    footer.style.borderTop = "1px solid #ddd";
    footer.style.display = "flex";
    footer.style.justifyContent = "flex-end";

    const btn = document.createElement("button");
    btn.id = "close-dep-bottom";
    btn.textContent = "close";
    btn.style.background = "#ddd";
    btn.style.border = "none";
    btn.style.padding = "6px 10px";
    btn.style.cursor = "pointer";
    btn.style.borderRadius = "6px";

    btn.addEventListener("click", () => {
      depPanel.style.display = "none";
      const network = window.network || window.networkBody?.network;
      clearFunctionHighlight(network);
    });

    footer.appendChild(btn);
    depPanel.appendChild(footer);
  }

  return depPanel;
}

// ------------------------
// backend fetch helpers
// ------------------------

async function fetchPackageDescription(pkg) {
  const key = (pkg || "").trim();
  if (!key) return "";

  if (pkgDescCache[key] !== undefined) return pkgDescCache[key];

  const resp = await fetch("/describe_package", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ package: key }),
  });
  if (!resp.ok) throw new Error(`HTTP ${resp.status}`);

  const data = await resp.json();
  const desc = data.description || "";
  pkgDescCache[key] = desc;
  return desc;
}

// ------------------------
// graph highlight helpers
// IMPORTANT: never write `hidden` in updates. slider/toggle owns that.
// ------------------------

function resetGraphStyles(network) {
  if (!network) return;

  currentSelectedNodeId = null;
  currentNodeChainVisited = new Set();
  currentNodeChainEdges = new Set();
  currentFunctionDepPkgs = new Set();

  const edges = network.body.data.edges.get();
  const nodes = network.body.data.nodes.get();

  network.body.data.edges.update(
    edges.map((e) => ({
      id: e.id,
      color: e.title === "LinkingTo" ? "#ff5b02" : "#999",
      width: 1,
      opacity: 1.0,
    }))
  );

  network.body.data.nodes.update(
    nodes.map((n) => ({
      id: n.id,
      opacity: 1.0,
      borderWidth: 1,
    }))
  );
}

function getDependencyChain(nodeId, edges) {
  const visited = new Set([nodeId]);
  const chainEdges = new Set();

  // don't share visited between directions; it blocks traversal
  function traverseDown(id, seenDown) {
    if (seenDown.has(id)) return;
    seenDown.add(id);
    edges.forEach((e) => {
      if (e.from === id && e.title !== "LinkingTo") {
        chainEdges.add(e.id);
        visited.add(e.to);
        traverseDown(e.to, seenDown);
      }
    });
  }

  function traverseUp(id, seenUp) {
    if (seenUp.has(id)) return;
    seenUp.add(id);
    edges.forEach((e) => {
      if (e.to === id && e.title !== "LinkingTo") {
        chainEdges.add(e.id);
        visited.add(e.from);
        traverseUp(e.from, seenUp);
      }
    });
  }

  traverseDown(nodeId, new Set());
  traverseUp(nodeId, new Set());

  return { visited, chainEdges };
}

function applyNodeChainHighlight(network) {
  if (!network || !currentSelectedNodeId) return;

  const edges = network.body.data.edges.get();
  const nodes = network.body.data.nodes.get();

  // base: fade non-chain edges
  network.body.data.edges.update(
    edges.map((e) => ({
      id: e.id,
      color: e.title === "LinkingTo" ? "#ff5b02" : "#999",
      width: 1,
      opacity: currentNodeChainEdges.has(e.id) ? 1.0 : 0.12,
    }))
  );

  // overlay: chain in red
  network.body.data.edges.update(
    edges
      .filter((e) => currentNodeChainEdges.has(e.id))
      .map((e) => ({
        id: e.id,
        color: "red",
        width: 3,
        opacity: 1.0,
      }))
  );

  // fade unrelated nodes (not too low or it looks like they're gone)
  network.body.data.nodes.update(
    nodes.map((n) => ({
      id: n.id,
      opacity: currentNodeChainVisited.has(n.id) ? 1.0 : 0.25,
      borderWidth: 1,
    }))
  );
}

function clearFunctionHighlight(network) {
  if (!network) return;
  currentFunctionDepPkgs = new Set();
  // go back to whatever the node selection view is
  if (currentSelectedNodeId) applyNodeChainHighlight(network);
  else resetGraphStyles(network);
}

function applyFunctionHighlight(network) {
  if (!network || currentFunctionDepPkgs.size === 0) return;

  const nodes = network.body.data.nodes.get();
  const edges = network.body.data.edges.get();

  // nodeId set for dependency packages
  const depNodeIds = new Set();
  nodes.forEach((n) => {
    const pkg = (n.label || "").trim();
    if (currentFunctionDepPkgs.has(pkg)) depNodeIds.add(n.id);
  });

  // edges fully inside dep set (blue)
  const blueEdgeIds = new Set();
  edges.forEach((e) => {
    if (depNodeIds.has(e.from) && depNodeIds.has(e.to)) blueEdgeIds.add(e.id);
  });

  // nodes: fade everything not in function dep set
  network.body.data.nodes.update(
    nodes.map((n) => ({
      id: n.id,
      opacity: depNodeIds.has(n.id) ? 1.0 : 0.12,
      borderWidth: depNodeIds.has(n.id) ? 2 : 1,
    }))
  );

  // edges: faint baseline, then overlay blue
  network.body.data.edges.update(
    edges.map((e) => ({
      id: e.id,
      color: e.title === "LinkingTo" ? "#ff5b02" : "#999",
      width: 1,
      opacity: depNodeIds.has(e.from) && depNodeIds.has(e.to) ? 0.9 : 0.06,
    }))
  );

  network.body.data.edges.update(
    edges
      .filter((e) => blueEdgeIds.has(e.id))
      .map((e) => ({
        id: e.id,
        color: "#1f6feb",
        width: 4,
        opacity: 1.0,
      }))
  );
}

// ------------------------
// list rendering
// ------------------------

function renderList(filteredFuncs, listEl, pkg, network) {
  const depPanel = document.getElementById("dep-panel");
  const depContent = document.getElementById("dep-content");

  listEl.innerHTML = "";
  depPanel.style.display = "none";

  filteredFuncs.forEach((fn) => {
    const li = document.createElement("li");
    li.textContent = fn;
    li.style.borderBottom = "1px solid #ddd";
    li.style.padding = "2px 0";
    li.style.cursor = "pointer";

    li.addEventListener("click", async () => {
      // selecting a function starts clean
      clearFunctionHighlight(network);

      listEl.querySelectorAll("li").forEach((el) => {
        el.style.backgroundColor = "";
        el.style.fontWeight = "normal";
      });
      li.style.backgroundColor = "#e0e0e0";
      li.style.fontWeight = "bold";

      depPanel.style.display = "block";
      depContent.innerHTML = `
        <div id="dep-scroll" style="
          max-height: 180px;
          overflow-y: auto;
          padding-right: 6px;
          margin-bottom: 8px;
          border-bottom: 1px solid #ccc;
        ">Loading dependencies...</div>
        <div id="func-desc" style="font-size: 12px; color: #444; padding-top: 6px; padding-bottom: 8px;">
          Fetching function description...
        </div>
      `;

      try {
        const depResp = await fetch("/analyze", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn, packages: pkg }),
        });
        if (!depResp.ok) throw new Error(`HTTP ${depResp.status}`);
        const depData = await depResp.json();

        const scrollEl = depContent.querySelector("#dep-scroll");
        const reqPkgs = (depData.required_packages || [])
          .map((p) => (p || "").trim())
          .filter(Boolean);

        if (reqPkgs.length > 0) {
          scrollEl.innerHTML = `
            <strong><span style="color: red">${fn}</span></strong> depends on:<br>
            <ul style="margin-top:4px; padding-left:18px;">
              ${reqPkgs.map((p) => `<li>${p}</li>`).join("")}
            </ul>
          `;

          // include root pkg so it doesn't get faded out
          currentFunctionDepPkgs = new Set([pkg, ...reqPkgs]);

          // fade non-deps + blue edges
          applyFunctionHighlight(network);
        } else {
          scrollEl.textContent = `${fn} has no detected dependencies.`;
          clearFunctionHighlight(network);
        }

        const descResp = await fetch("/describe_function", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn, package: pkg }),
        });
        if (!descResp.ok) throw new Error(`HTTP ${descResp.status}`);

        const descData = await descResp.json();
        const descEl = depContent.querySelector("#func-desc");
        descEl.innerHTML = descData.description
          ? `<div style="margin-top:4px; margin-bottom: 4px">${descData.description}</div>`
          : "<i>No description available.</i>";
      } catch (err) {
        console.error("Failed to load data:", err);
        depContent.textContent = "Failed to load function info.";
        clearFunctionHighlight(network);
      }
    });

    listEl.appendChild(li);
  });
}

async function loadPackageFunctions() {
  try {
    const resp = await fetch("/data/package_functions.json");
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);

    const data = await resp.json();

    Object.keys(data).forEach((pkg) => {
      data[pkg] = data[pkg].filter(
        (func) =>
          !func.startsWith(".__C__") &&
          !func.startsWith(".__T__") &&
          !func.startsWith(".") &&
          !func.startsWith("%") &&
          !func.includes("<-") &&
          !func.startsWith("_") &&
          !(func[0] == func[0].toUpperCase())
      );
    });

    return data;
  } catch (e) {
    console.error("Failed to load function data:", e);
    return {};
  }
}

// ------------------------
// init + events
// ------------------------

async function init(network) {
  mountDepPanelIntoSidebar();
  ensurePackagePanel();

  packageData = await loadPackageFunctions();

  network.on("selectNode", (params) => {
    if (params.nodes.length === 0) return;

    const nodeId = params.nodes[0];
    const node = network.body.data.nodes.get(nodeId);
    const pkg = (node?.label || "").trim();
    if (!pkg) return;

    currentSelectedNodeId = nodeId;
    currentFunctionDepPkgs = new Set(); // selecting package resets function mode

    const allEdges = network.body.data.edges.get();
    const { visited, chainEdges } = getDependencyChain(nodeId, allEdges);

    currentNodeChainVisited = visited;
    currentNodeChainEdges = chainEdges;

    applyNodeChainHighlight(network);

    const nameEl = document.getElementById("package-name");
    const listEl = document.getElementById("function-list");
    const searchEl = document.getElementById("function-search");

    nameEl.textContent = pkg;
    const funcs = packageData[pkg] || [];

    if (funcs.length === 0) {
      listEl.innerHTML =
        "<li style='color:#999;'>No functions found or not available.</li>";
    } else {
      renderList(funcs, listEl, pkg, network);
    }

    searchEl.oninput = () => {
      const q = searchEl.value.toLowerCase();
      const filtered = funcs.filter((f) => f.toLowerCase().includes(q));
      renderList(filtered, listEl, pkg, network);
    };

    // package panel
    const pkgPanel = document.getElementById("pkg-panel");
    const pkgNameEl = document.getElementById("pkg-panel-name");
    const pkgDescEl = document.getElementById("pkg-panel-desc");

    pkgPanel.style.display = "block";
    pkgNameEl.textContent = pkg;
    pkgDescEl.innerHTML = "<i>Loading package description...</i>";

    fetchPackageDescription(pkg)
      .then((desc) => {
        pkgDescEl.innerHTML = desc
          ? `<strong>Description</strong><div style="margin-top:4px;">${desc}</div>`
          : "<i>No description available.</i>";
      })
      .catch((err) => {
        console.error("Failed to load package description:", err);
        pkgDescEl.innerHTML = "<i>Failed to load package description.</i>";
      });
  });

  network.on("deselectNode", () => {
    resetGraphStyles(network);

    const pkgPanel = document.getElementById("pkg-panel");
    if (pkgPanel) pkgPanel.style.display = "none";

    const depPanel = document.getElementById("dep-panel");
    if (depPanel) depPanel.style.display = "none";
  });
}

// ------------------------
// startup
// ------------------------

window.addEventListener("load", () => {
  const network = window.network || window.networkBody?.network;
  if (!network) {
    console.warn("Network object not found — layer slider disabled.");
    return;
  }

  init(network);

  const detectedMax =
    typeof MAX_LAYER_DEPTH !== "undefined" ? MAX_LAYER_DEPTH : 5;

  const panel = document.createElement("div");
  panel.id = "layer-slider-panel";
  panel.style.position = "absolute";
  panel.style.bottom = "15px";
  panel.style.left = "15px";
  panel.style.backgroundColor = "rgba(255,255,255,0.9)";
  panel.style.padding = "8px 12px";
  panel.style.border = "1px solid #ccc";
  panel.style.borderRadius = "8px";
  panel.style.boxShadow = "0 2px 4px rgba(0,0,0,0.2)";
  panel.style.fontFamily = "Arial, sans-serif";
  panel.style.fontSize = "13px";
  panel.style.zIndex = 9999;

  panel.innerHTML = `
    <label for="layer-slider"><strong>Dependency Layer Depth:</strong></label><br>
    <input type="range" id="layer-slider" min="0" max="${detectedMax}" value="${detectedMax}" style="width:150px;">
    <span id="layer-value">${detectedMax}</span>
    <hr style="margin:8px 0;">
    <label><input type="checkbox" id="same-level-toggle" checked> Show same-level dependencies</label>
  `;

  document.body.appendChild(panel);

  const slider = document.getElementById("layer-slider");
  const layerValue = document.getElementById("layer-value");

  slider.addEventListener("input", () => {
    const maxLayer = parseInt(slider.value);
    layerValue.textContent = maxLayer;

    const nodes = network.body.data.nodes.get();
    nodes.forEach((n) => {
      const layer = parseInt(n.title.match(/Layer: (\d+)/)?.[1] || "0");
      n.hidden = layer > maxLayer;
    });
    network.body.data.nodes.update(nodes);

    // re-apply current view after hide/show
    if (currentFunctionDepPkgs.size > 0) applyFunctionHighlight(network);
    else if (currentSelectedNodeId) applyNodeChainHighlight(network);
  });

  const sameToggle = document.getElementById("same-level-toggle");
  sameToggle.addEventListener("change", () => {
    const show = sameToggle.checked;
    const edges = network.body.data.edges.get();
    edges.forEach((e) => {
      if (e.same_level === true) e.hidden = !show;
    });
    network.body.data.edges.update(edges);

    if (currentFunctionDepPkgs.size > 0) applyFunctionHighlight(network);
    else if (currentSelectedNodeId) applyNodeChainHighlight(network);
  });

  network.on("doubleClick", (params) => {
    if (params.nodes.length === 0) return;
    const nodeId = params.nodes[0];
    const node = network.body.data.nodes.get(nodeId);
    if (!node || !node.label) return;
    const pkg = node.label.trim();
    const url = `https://cran.r-project.org/package=${encodeURIComponent(pkg)}`;
    window.open(url, "_blank");
  });
});
