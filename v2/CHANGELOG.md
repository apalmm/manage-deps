# Multinode DeepDep: A Python Port of deepdep

_(Dependency Visualization and Analysis Toolkit)_

---

### **v0.1 — Initial PyVis Prototype**

- Implemented a working translation of deepdeps `plot_dependencies()` using `NetworkX` + `PyVis`.
- Generated interactive HTML graphs with one node per package and one edge per dependency.
- Color-coded edges by dependency type (`Imports`, `LinkingTo`).

---

### **v0.2 — Layered Layout (deepdep-style)**

- Added BFS-based `assign_layers()` to compute dependency depth.
- Introduced per-layer color palette.
- Added `same_level` and `reverse` parameters for edge filtering (similar to deepdep)

---

### **v0.3 — Static Matplotlib Version**

- Built a concentric-ring Matplotlib version for publication figures.
- Used for static, non-interactive deepdep-like circular plots. Didn't like this

---

### **v0.4 — Interactive Hierarchical Layout**

- Restored PyVis interactivity with `force_atlas_2based` physics which keeps a more circular form.
- Added hover tooltips and improved edge styling.

---

### **v0.5 — Layout Tuning & Label Readability**

- Tuned physics parameters for stable spacing (`gravity`, `spring_length`, `damping`).
- Enlarged canvas to 100vh.
- Increased font size and contrast for labels.

---

### **v0.6 — Edge Filtering: same_level = False**

- Added `remove_same_level_edges()` to drop edges within the same layer.

---

### **v0.7 — Reverse & Transitive Edge Cleanup**

- Added optional `remove_transitive_edges()` to remove redundant indirect edges.
- Resulted in much cleaner, directional graphs.

---

### **v0.8 — Core Dependency Highlighting**

- Calculated in-degree metrics for each node.
- Scaled node size by relative importance.

---

### **v0.9 — CLI-Driven Titles**

- Tried adding `title` argument to `visualize_graph()`, failed need to figure out a way to add good titles.

---

### **v0.10 — Added title and timestamp to graph**

- Added a title and timestamp to the resulting graph using BeautifulSoup to edit the HTML directly through python.
- Also added max layer slider to help isolate / view specific layers in the graph.
- Added double click feature on node to pull up CRAN page for more information about the package.
- Added "tip" for interacting with a node, may update this to be calling CreatePackageReport(pkg) instead of the CRAN url.

---

### **v1.0 — Added search bar and function lookup for each node**

- Initalized versioning with github repo
- Make sure to start python server to avoid CORS issues and navigate to root directory
- command is `python3 -m http.server 8000`

---

### **v1.0.1 — Transition to flask**

- Transitioning program to flask
- Adding in better file names for description
- added app.py and automatic window opening
- Graphs now named based on timestamp as to avoid overwriting (might change this back)

---

### **v1.0.12 — Flask Backend, Fetching Functions**

- Transitioned program to flask
- Successfully can call function analyze endpoint
- Setting depth automatically as max depth, changing color of bolded lines when node selected
- run with `python3 -m myapp [pkgs]`

---
