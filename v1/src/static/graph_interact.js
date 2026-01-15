let packageData = {}; // make it global so the event handler can access it

function renderList(filteredFuncs, listEl) {
  filteredFuncs.forEach((fn) => {
    const li = document.createElement("li");
    li.textContent = fn;
    li.style.borderBottom = "1px solid #ddd";
    li.style.padding = "2px 0";
    li.style.cursor = "pointer";

    //highlight on click
    li.addEventListener("click", () => {
      listEl.querySelectorAll("li").forEach((el) => {
        //reset styles when not clicked
        el.style.backgroundColor = "";
        li.style.fontWeight = "None";
      });
      li.style.backgroundColor = "#e0e0e0";
      li.style.fontWeight = "bold";
    });

    listEl.appendChild(li);
  });
}

async function loadPackageFunctions() {
  try {
    const resp = await fetch("/data/package_functions.json");
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    return await resp.json();
  } catch (e) {
    console.error("Failed to load function data:", e);
    return {};
  }
}

async function init(network) {
  // Load package-function data
  packageData = await loadPackageFunctions();

  // Only attach the event listener *after* data is ready
  network.on("selectNode", function (params) {
    if (params.nodes.length === 0) return;

    const nodeId = params.nodes[0];
    const node = network.body.data.nodes.get(nodeId);
    const pkg = node.label;

    const nameEl = document.getElementById("package-name");
    const listEl = document.getElementById("function-list");
    const searchEl = document.getElementById("function-search");

    nameEl.textContent = pkg;
    listEl.innerHTML = "";

    const funcs = packageData[pkg] || [];
    if (funcs.length === 0) {
      listEl.innerHTML =
        //current issue I think loading window before data is ready
        "<li style='color:#999;'>No functions found or not available.</li>";
    }

    console.log(listEl);
    //inital list render
    renderList(funcs, listEl);

    //filter as user types
    searchEl.addEventListener("input", () => {
      const q = searchEl.value.toLowerCase();
      const filtered = funcs.filter((f) => f.toLowerCase().includes(q));
      renderList(filtered);
    });
  });
}

//start everything
window.addEventListener("load", function () {
  //once our window mounts, we can try to get the network object from pyvis
  const network = window.network || window.networkBody?.network;

  if (!network) {
    console.warn("Network object not found — layer slider disabled."); //sanity check
    return;
  }

  init(network); //initialize package function loading and event handling

  const detectedMax =
    typeof MAX_LAYER_DEPTH !== "undefined" ? MAX_LAYER_DEPTH : 5; //have we input a cmndline arg for max depth??, default to 5

  //MAX slider container and styling
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

  //insert it into the document body
  document.body.appendChild(panel);

  //js behavior for the slider
  const slider = document.getElementById("layer-slider");
  const layerValue = document.getElementById("layer-value");

  slider.addEventListener("input", () => {
    const maxLayer = parseInt(slider.value);
    layerValue.textContent = maxLayer;
    const nodes = network.body.data.nodes.get(); //get set of nodes
    nodes.forEach((n) => {
      //use a regular expression to search for a pattern inside that node string
      const layer = parseInt(n.title.match(/Layer: (\d+)/)?.[1] || "0");
      n.hidden = layer > maxLayer;
    });
    network.body.data.nodes.update(nodes); //update the network with the modified nodes based on slider change
  });

  //show/hide same-level dependencies
  const sameToggle = document.getElementById("same-level-toggle");
  sameToggle.addEventListener("change", () => {
    const show = sameToggle.checked;
    const edges = network.body.data.edges.get();
    edges.forEach((e) => {
      if (e.same_level === true) {
        e.hidden = !show;
      }
    });
    network.body.data.edges.update(edges);
    console.log(edges);
    console.log(show);
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

  //open CRAN page on double click
  network.on("doubleClick", function (params) {
    if (params.nodes.length === 0) return; //clicked empty space
    const nodeId = params.nodes[0];
    const node = network.body.data.nodes.get(nodeId);
    if (!node || !node.label) return; //no valid node found or no label, ignore double click
    const pkg = node.label.trim();
    const url = `https://cran.r-project.org/package=${encodeURIComponent(pkg)}`;
    window.open(url, "_blank"); //append to cran url and open in new tab
  });
});
