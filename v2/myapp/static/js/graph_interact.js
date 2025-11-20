let packageData = {}; //global so other handlers can use it

function renderList(filteredFuncs, listEl, pkg) {
  const depPanel = document.getElementById("dep-panel");
  const depContent = document.getElementById("dep-content");
  const closeDep = document.getElementById("close-dep");

  listEl.innerHTML = "";
  depPanel.style.display = "none"; //hide when rerendering

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
      depContent.textContent = "loading...";
      try {
        const resp = await fetch("/analyze", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn, packages: pkg }),
        });

        if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
        const data = await resp.json();

        if (data.required_packages && data.required_packages.length > 0) {
          depContent.innerHTML = `
            <strong>${fn}</strong> depends on:<br>
            <ul style="margin-top:4px; padding-left:18px;">
              ${data.required_packages.map((pkg) => `<li>${pkg}</li>`).join("")}
            </ul>
          `;
        } else {
          depContent.textContent = `${fn} has no detected dependencies.`;
        }
      } catch (err) {
        console.error("failed to fetch backend data:", err);
        depContent.textContent = "failed to load dependencies.";
      }
    });

    listEl.appendChild(li);
  });

  if (closeDep && !closeDep.dataset.bound) {
    closeDep.dataset.bound = "true";
    closeDep.addEventListener("click", () => {
      depPanel.style.display = "none";
    });
  }
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
          !func.startsWith(".")
      );
    });

    return data;
  } catch (e) {
    console.error("Failed to load function data:", e);
    return {};
  }
}

async function init(network) {
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
  });
}

//startup
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

  //improved discrete legend panel
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
      <div style="font-size:11px;margin-top:2px;">Layer (0)</div>
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
