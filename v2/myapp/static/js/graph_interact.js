let packageData = {}; // global so other handlers can use it
let pkgDescCache = {}; // cache package -> description so node clicks don't spam backend

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
  panel.style.display = "none"; // shown on node select
  panel.style.maxHeight = "32%";

  panel.innerHTML = `
    <div style="position:sticky;top:0;background:#f9f9f9;z-index:2;padding: 14px 16px;border-bottom:1px solid #ddd;">
      <h2 style="margin:0;">Package Info</h2>
      <div id="pkg-panel-name" style="margin-top:6px;font-weight:bold;font-size:14px;"></div>
    </div>
    <div id="pkg-panel-desc" style="color:#333; padding: 12px 16px; font-size: 12px; line-height: 1.35;">
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

  // remove absolute positioning so it stacks under pkg-panel
  depPanel.style.position = "relative";
  depPanel.style.top = "";
  depPanel.style.left = "";
  depPanel.style.right = "";
  depPanel.style.marginLeft = "0";
  depPanel.style.width = "100%";
  depPanel.style.height = "auto";
  depPanel.style.maxHeight = "48%"; // keeps it from taking over the whole column

  // make sure it is inside the sidebar
  if (depPanel.parentElement !== sidebar) {
    sidebar.appendChild(depPanel);
  }

  // optional: hide the top close button (since we add a bottom-right one)
  const closeTop = document.getElementById("close-dep");
  if (closeTop) closeTop.style.display = "none";

  // add a sticky footer close button bottom-right (only once)
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
    });

    footer.appendChild(btn);
    depPanel.appendChild(footer);
  }

  return depPanel;
}

async function fetchPackageDescription(pkg) {
  const key = (pkg || "").trim();
  if (!key) return "";

  if (pkgDescCache[key]) return pkgDescCache[key];

  const resp = await fetch("/describe_package", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ package: key }),
  });
  if (!resp.ok) throw new Error(`HTTP ${resp.status}`);

  const data = await resp.json();
  const desc = data.description || "";

  // cache whatever we got (including empty), so repeated clicks are fast
  pkgDescCache[key] = desc;
  return desc;
}

function renderList(filteredFuncs, listEl, pkg) {
  const depPanel = document.getElementById("dep-panel");
  const depContent = document.getElementById("dep-content");

  listEl.innerHTML = "";
  depPanel.style.display = "none"; // hide when rerendering

  filteredFuncs.forEach((fn) => {
    const li = document.createElement("li");
    li.textContent = fn;
    li.style.borderBottom = "1px solid #ddd";
    li.style.padding = "2px 0";
    li.style.cursor = "pointer";

    li.addEventListener("click", async () => {
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
        <div id="func-desc" style="font-size: 12px; color: #444; padding-top: 6px;">
          Fetching function description...
        </div>
      `;

      try {
        // fetch dependencies
        const depResp = await fetch("/analyze", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn, packages: pkg }),
        });
        if (!depResp.ok) throw new Error(`HTTP ${depResp.status}`);
        const depData = await depResp.json();

        const scrollEl = depContent.querySelector("#dep-scroll");
        if (depData.required_packages && depData.required_packages.length > 0) {
          scrollEl.innerHTML = `
            <strong>${pkg} → <span style="color: red">${fn}</span></strong> depends on:<br>
            <ul style="margin-top:4px; padding-left:18px;">
              ${depData.required_packages.map((p) => `<li>${p}</li>`).join("")}
            </ul>
          `;
        } else {
          scrollEl.textContent = `${fn} has no detected dependencies.`;
        }

        // fetch function description (new endpoint)
        const descResp = await fetch("/describe_function", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn, package: pkg }),
        });
        if (!descResp.ok) throw new Error(`HTTP ${descResp.status}`);

        const descData = await descResp.json();
        const descEl = depContent.querySelector("#func-desc");
        descEl.innerHTML = descData.description
          ? `<strong>${pkg}::${fn}</strong><div style="margin-top:4px;">${descData.description}</div>`
          : "<i>No description available.</i>";
      } catch (err) {
        console.error("Failed to load data:", err);
        depContent.textContent = "Failed to load function info.";
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

async function init(network) {
  mountDepPanelIntoSidebar(); // prevents overlap with package panel
  ensurePackagePanel(); // create package panel once

  packageData = await loadPackageFunctions();

  // recursive dependency chain traversal
  function getDependencyChain(nodeId, edges) {
    const visited = new Set();
    const chainEdges = new Set();

    function traverseDown(id) {
      if (visited.has(id)) return;
      visited.add(id);
      edges.forEach((e) => {
        if (e.from === id && e.title !== "LinkingTo") {
          chainEdges.add(e.id);
          traverseDown(e.to);
        }
      });
    }

    function traverseUp(id) {
      if (visited.has(id)) return;
      visited.add(id);
      edges.forEach((e) => {
        if (e.to === id && e.title !== "LinkingTo") {
          chainEdges.add(e.id);
          traverseUp(e.from);
        }
      });
    }

    traverseDown(nodeId);
    traverseUp(nodeId);
    return { visited, chainEdges };
  }

  network.on("selectNode", (params) => {
    if (params.nodes.length === 0) return;

    const nodeId = params.nodes[0];
    const node = network.body.data.nodes.get(nodeId);
    const pkg = node.label;

    const allEdges = network.body.data.edges.get();
    const allNodes = network.body.data.nodes.get();

    const { visited, chainEdges } = getDependencyChain(nodeId, allEdges);

    // reset all edges first
    const resetEdges = allEdges.map((e) => ({
      id: e.id,
      color: e.title === "LinkingTo" ? "#ff5b02" : "#999",
      width: 1,
      opacity: chainEdges.has(e.id) ? 1.0 : 0.1,
    }));

    // highlight dependency edges in red
    const highlightedEdges = allEdges
      .filter((e) => chainEdges.has(e.id))
      .map((e) => ({
        id: e.id,
        color: "red",
        width: 3,
        opacity: 1.0,
      }));

    network.body.data.edges.update(resetEdges);
    network.body.data.edges.update(highlightedEdges);

    // fade unrelated nodes
    const updatedNodes = allNodes.map((n) => ({
      id: n.id,
      opacity: visited.has(n.id) ? 1.0 : 0.2,
      borderWidth: 1,
    }));
    network.body.data.nodes.update(updatedNodes);

    // update function list
    const nameEl = document.getElementById("package-name");
    const listEl = document.getElementById("function-list");
    const searchEl = document.getElementById("function-search");

    nameEl.textContent = pkg;
    const funcs = packageData[pkg] || [];

    if (funcs.length === 0) {
      listEl.innerHTML =
        "<li style='color:#999;'>No functions found or not available.</li>";
    } else {
      renderList(funcs, listEl, pkg);
    }

    searchEl.oninput = () => {
      const q = searchEl.value.toLowerCase();
      const filtered = funcs.filter((f) => f.toLowerCase().includes(q));
      renderList(filtered, listEl, pkg);
    };

    // show + populate package panel (separate)
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
    const edges = network.body.data.edges.get();
    const nodes = network.body.data.nodes.get();

    const resetEdges = edges.map((e) => ({
      id: e.id,
      color: e.title === "LinkingTo" ? "#ff5b02" : "#999",
      width: 1,
      opacity: 1.0,
    }));

    const resetNodes = nodes.map((n) => ({
      id: n.id,
      opacity: 1.0,
      borderWidth: 1,
    }));

    network.body.data.edges.update(resetEdges);
    network.body.data.nodes.update(resetNodes);

    // hide package panel on deselect
    const pkgPanel = document.getElementById("pkg-panel");
    if (pkgPanel) pkgPanel.style.display = "none";
  });
}

// startup
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
  });

  const sameToggle = document.getElementById("same-level-toggle");
  sameToggle.addEventListener("change", () => {
    const show = sameToggle.checked;
    const edges = network.body.data.edges.get();
    edges.forEach((e) => {
      if (e.same_level === true) e.hidden = !show;
    });
    network.body.data.edges.update(edges);
  });

  // improved discrete legend panel
  const legend = document.createElement("div");
  legend.id = "graph-legend";
  legend.style.position = "absolute";
  legend.style.bottom = "6%";
  legend.style.right = "15px";
  legend.style.backgroundColor = "rgba(255,255,255,0.97)";
  legend.style.padding = "14px 16px";
  legend.style.border = "1px solid #ccc";
  legend.style.borderRadius = "10px";
  legend.style.boxShadow = "0 2px 6px rgba(0,0,0,0.25)";
  legend.style.fontFamily = "Arial, sans-serif";
  legend.style.fontSize = "13px";
  legend.style.zIndex = 9999;
  legend.innerHTML = `
  <strong style="font-size:14px;">Legend</strong>
  <div style="margin-top:10px;margin-bottom:6px;">
    <span style="display:inline-block;width:24px;height:3px;background:#ff5b02;margin-right:6px;"></span>
    <strong>LinkingTo</strong> → compiled dependency (C/C++ interface between packages)
  </div>
  <div style="margin-bottom:6px;">
    <span style="display:inline-block;width:24px;height:3px;background:#999;margin-right:6px;"></span>
    <strong>Imports</strong> → standard dependency used by package functions
  </div>
  <div style="margin-bottom:10px;">
    <span style="display:inline-block;width:24px;height:3px;background:red;margin-right:6px;"></span>
    <strong>Highlighted path</strong> → active dependency chain from the selected node
  </div>
  <hr style="margin:8px 0;">
  <div style="margin-bottom:12px;font-weight:bold;">Node color by dependency layer depth</div>
  <div style="display:flex;justify-content:space-around;align-items:end;gap:6px;text-align:center; margin-bottom:12px;">
    <div>
      <div style="width:18px;height:18px;background:#5fc8f4;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;"><b>Root package</b></div>
    </div>
    <div>
      <div style="width:18px;height:18px;background:#a1ce40;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;">Layer (1)</div>
    </div>
    <div>
      <div style="width:18px;height:18px;background:#fde74c;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;">Layer (2)</div>
    </div>
    <div>
      <div style="width:18px;height:18px;background:#ff8330;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;">Layer (3)</div>
    </div>
    <div>
      <div style="width:18px;height:18px;background:#e55934;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;">Layer (4)</div>
    </div>
    <div>
      <div style="width:18px;height:18px;background:#7b5e7b;border:1px solid #666;border-radius:50%;margin:auto;"></div>
      <div style="font-size:11px;margin-top:2px;">Layer (5+)</div>
    </div>
  </div>
  <div style="font-size:12px;text-align:center;margin-top:6px;color:#444;">Root Package → Deeper dependency layers</div>
  <hr style="margin:10px 0;">
  <div><strong>Node size</strong> → larger = higher dependency importance</div>
`;
  document.body.appendChild(legend);

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
