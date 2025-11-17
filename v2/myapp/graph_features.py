from bs4 import BeautifulSoup
from datetime import datetime


def add_features(filename, title="", max_depth=5):
    # read html content
    with open(filename, "r", encoding="utf-8") as f:
        html_content = f.read()

    soup = BeautifulSoup(html_content, "lxml")

    # add title overlay
    graph_title = soup.new_tag("div")
    graph_title.string = title
    graph_title["style"] = (
        "position:absolute;"
        "top:15px;"
        "left:0;"
        "width:100%;"
        "text-align:center;"
        "font-family:Arial,sans-serif;"
        "font-size:24px;"
        "font-weight:bold;"
        "color:#222;"
        "background:rgba(255,255,255,0.8);"
        "padding:8px 0;"
        "z-index:10000;"
        "border-bottom:1px solid #ccc;"
    )

    # add timestamp overlay
    timestamp_div = soup.new_tag("div")
    timestamp_text = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    timestamp_div.string = timestamp_text
    timestamp_div["style"] = (
        "position:absolute;"
        "bottom:10px;"
        "right:15px;"
        "font-family:Arial,sans-serif;"
        "font-size:12px;"
        "color:#555;"
        "background:rgba(255,255,255,0.8);"
        "padding:4px 8px;"
        "border-radius:6px;"
        "z-index:10000;"
        "border:1px solid #ddd;"
    )

    # insert overlays
    body_tag = soup.body
    if body_tag is not None:
        first_div = body_tag.find("div")
        if first_div:
            first_div.insert_before(graph_title)
        else:
            body_tag.insert(0, graph_title)
        body_tag.append(timestamp_div)

    # write updated html back
    with open(filename, "w", encoding="utf-8") as f:
        f.write(str(soup))

    # add layer slider + js hook
    add_layer_slider(filename, "static/js/graph_interact.js", max_depth)
    add_hint(filename)
    add_info_panel(filename)
    # add_dep_panel(filename)


def add_layer_slider(html_path, js_path="static/js/graph_interact.js", max_depth=5):
    # path is static now so flask can serve it
    script_tag = (
        f'\n<script type="text/javascript">'
        f"const MAX_LAYER_DEPTH={max_depth};"
        f"const ENABLE_SAME_LEVEL_TOGGLE=true;"
        f"</script>\n"
        f'<script type="text/javascript" src="/{js_path}"></script>\n'
    )

    with open(html_path, "a", encoding="utf-8") as f:
        f.write(script_tag)


def add_dep_panel(filename):
    dep_panel_html = """
      <div id="dep-panel" style="
          position:absolute;
          top:70%;
          right:50px;
          width:20%;
          height:18%;
          background:#fefefe;
          border-left:2px solid #ccc;
          border-top:1px solid #ccc;
          overflow-y:auto;
          font-family:Arial,sans-serif;
          box-shadow:-3px 0 8px rgba(0,0,0,0.15);
          border-radius:8px;
          padding:10px;
          display:none;
          z-index:999999;
          pointer-events:auto;
      ">
        <div style="position:sticky;top:0;background:#fff;padding-bottom:4px;">
          <h3 style="margin-top:0;">Direct Package Dependencies</h3>
          <button id="close-dep" style="
            float:right;
            background:none;
            border:none;
            cursor:pointer;
            font-size:14px;">✕</button>
        </div>
        <div id="dep-content" style="font-size:13px;color:#333;"></div>
      </div>
    """
    with open(filename, "a", encoding="utf-8") as f:
        f.write(dep_panel_html)


def add_hint(filename):
    hint_html = """
        <div id="hint-box" style="
            bottom:10%;
            left:15px;
            position:absolute;
            background:rgba(255,255,255,0.9);
            color:#333;
            font-family:Arial,sans-serif;
            font-size:13px;
            padding:6px 10px;
            border-radius:8px;
            border:1px solid #ccc;
            box-shadow:0 1px 3px rgba(0,0,0,0.2);
            z-index:9999;
            margin-bottom:20px;
        ">
            💡 <b>tip:</b> double-click a node to open its cran page<br>
            🎚 use the slider to adjust dependency layer depth
        </div>
    """
    with open(filename, "a", encoding="utf-8") as f:
        f.write(hint_html)


def add_info_panel(filename):
    panel_html = """
        <div id="info-panel" style="
            position:absolute;
            top:10%;
            right:50px;
            width:20%;
            height:50%;
            background:#f9f9f9;
            border-left:2px solid #ccc;
            overflow-y:auto;
            font-family:Arial,sans-serif;
            box-shadow:-3px 0 8px rgba(0,0,0,0.1);
            z-index:9998;
            border-radius:8px;
            padding: 0 12px 12px 12px;
        ">
            <div id="info-header" style="position:sticky;top:0;background:white;z-index:2;padding-bottom:12px;">
                <h2 style="margin-top: 12px;">package info</h2>
                <input
                    type="text"
                    id="function-search"
                    placeholder="search functions..."
                    style="width:95%;padding:4px;margin-bottom:8px;"
                />
            </div>
            <p id="package-name" style="font-weight:bold;">select a node to see its function list</p>
            <ul id="function-list" style="list-style-type:none;padding-left:0;margin:0;"></ul>
        </div>

        <div id="dep-panel" style="
            position:absolute;
            top:10%;
            right: auto;
            margin-left: 50px;
            width:20%;
            height:50%;
            background:#ffffff;
            border-left:2px solid #ccc;
            overflow-y:auto;
            font-family:Arial,sans-serif;
            box-shadow:-3px 0 8px rgba(0,0,0,0.1);
            z-index:9999;
            border-radius:8px;
            padding:12px;
            display:none;
        ">
            <div id="dep-header" style="position:sticky;top:0;background:white;z-index:2;padding-bottom:6px;">
                <h2 style="margin-top:0;">function dependencies</h2>
                <button id="close-dep" style="background:#ddd;border:none;padding:4px 8px;cursor:pointer;border-radius:4px;">close</button>
            </div>
            <div id="dep-content" style="font-size:13px;color:#333;margin-top:8px;"></div>
        </div>
    """
    with open(filename, "a", encoding="utf-8") as f:
        f.write(panel_html)
