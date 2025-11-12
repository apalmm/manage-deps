// //load and display package functions
// async function loadPackageFunctions() {
//   //needs to be async to await fetch
//   try {
//     const resp = await fetch("data/package_functions.json");
//     return await resp.json();
//   } catch (e) {
//     //sanity check
//     console.error("Failed to load function data:", e);
//     return {};
//   }
// }

// async function init() {
//   const pkgFuncs = await loadPackageFunctions();
//   console.log(pkgFuncs);
// }

// init();

// network.on("selectNode", function (params) {
//   if (params.nodes.length === 0) return;
//   const nodeId = params.nodes[0];
//   const node = network.body.data.nodes.get(nodeId);
//   const pkg = node.label;

//   // const panel = document.getElementById("info-panel");
//   const nameEl = document.getElementById("package-name");
//   const listEl = document.getElementById("function-list");

//   nameEl.textContent = pkg;
//   listEl.innerHTML = "";

//   const funcs = packageData[pkg] || [];
//   if (funcs.length === 0) {
//     listEl.innerHTML =
//       "<li style='color:#999;'>No functions found or not available.</li>";
//   } else {
//     funcs.forEach((fn) => {
//       const li = document.createElement("li");
//       li.textContent = fn;
//       li.style.borderBottom = "1px solid #ddd";
//       li.style.padding = "2px 0";
//       listEl.appendChild(li);
//     });
//   }
// });

window.addEventListener("load", function () {
  //once our window mounts, we can try to get the network object from pyvis
  const network = window.network || window.networkBody?.network;

  if (!network) {
    console.warn("Network object not found — layer slider disabled."); //sanity check
    return;
  }

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
