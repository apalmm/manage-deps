let packageData = {}; // make it global so the event handler can access it

function renderList(filteredFuncs, listEl) {
  const depPanel = document.getElementById("dep-panel");
  const depContent = document.getElementById("dep-content");
  const closeDep = document.getElementById("close-dep");

  listEl.innerHTML = "";
  depPanel.style.display = "none"; //hide when re-rendering list

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
        console.log(fn);
        const resp = await fetch("/analyze", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ function: fn }),
        });

        if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
        const data = await resp.json();

        if (data.required_packages && data.required_packages.length > 0) {
          console.log(data.required_packages);
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

  //close button for dependency panel
  if (closeDep) {
    closeDep.addEventListener("click", () => {
      depPanel.style.display = "none";
    });
  }
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

async function loadFunctionDependencies() {
  try {
    const resp = await fetch("/analyze", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ func_name: selected }), // Replace with actual function name as needed
    });
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    const data = await resp.json();
  } catch (e) {
    console.error("Failed to load function dependencies:", e);
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
