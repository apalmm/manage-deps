from bs4 import BeautifulSoup
from datetime import datetime
import os

def add_features(filename, title="", max_depth=5):
    with open(filename, "r", encoding="utf-8") as f:
        html_content = f.read()

    soup = BeautifulSoup(html_content, "lxml")

    #add the title overlay (top center)
    graph_title = soup.new_tag("div")
    graph_title.string = title
    graph_title["style"] = (
        "position: absolute;"
        "top: 15px;"
        "left: 0;"
        "width: 100%;"
        "text-align: center;"
        "font-family: Arial, sans-serif;"
        "font-size: 24px;"
        "font-weight: bold;"
        "color: #222;"
        "background: rgba(255, 255, 255, 0.8);"
        "padding: 8px 0;"
        "z-index: 10000;"
        "border-bottom: 1px solid #ccc;"
    )

    #add the timestamp overlay (bottom right)
    timestamp_div = soup.new_tag("div")
    timestamp_text = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    timestamp_div.string = timestamp_text
    timestamp_div["style"] = (
        "position: absolute; "
        "bottom: 10px; "
        "right: 15px; "
        "font-family: Arial, sans-serif;"
        "font-size: 12px;"
        "color: #555;"
        "background: rgba(255, 255, 255, 0.8);"
        "padding: 4px 8px;"
        "border-radius: 6px;"
        "z-index: 10000;"
        "border: 1px solid #ddd;"
    )

    #ensure that we insert the title BEFORE the network container so it appeers at the top
    body_tag = soup.body
    if body_tag is not None:
        first_div = body_tag.find("div")
        if first_div:
            first_div.insert_before(graph_title)
        else:
            body_tag.insert(0, graph_title)
        #add a timestamp at the end of the body
        body_tag.append(timestamp_div)

    with open(filename, "w", encoding="utf-8") as f:
        f.write(str(soup))

    def add_layer_slider(html_path, js_path="src/static/graph_interact.js", max_depth=5):
        #find path for layer_slider.js relative to html_path
        rel_path = os.path.relpath(js_path, start=os.path.dirname(html_path))
        script_tag = (
            f'\n<script type="text/javascript">const MAX_LAYER_DEPTH = {max_depth}; const ENABLE_SAME_LEVEL_TOGGLE = true;</script>\n'
            f'<script type="text/javascript" src="{rel_path}"></script>\n'
        )

        #write to the file
        with open(html_path, "a", encoding="utf-8") as f:
            f.write(script_tag)

    def add_hint(filename):
        hint_html = """
            <div id="hint-box" style="
                bottom: 10%;
                left: 15px;
                position: absolute;
                background: rgba(255, 255, 255, 0.9);
                color: #333;
                font-family: Arial, sans-serif;
                font-size: 13px;
                padding: 6px 10px;
                border-radius: 8px;
                border: 1px solid #ccc;
                box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                z-index: 9999;
                margin-bottom: 20px;
                ">
                💡 <b>Tip:</b> Double-click a node to open its CRAN page<br>
                🎚 Use the slider to adjust dependency layer depth
            </div>
        """
        with open(filename, "a", encoding="utf-8") as f:
            f.write(hint_html)  

    def add_info_panel(filename):
        package_info_panel = """
            <div id="info-panel" style="
                position: absolute;
                top: 0;
                right: 0;
                width: 25%;
                height: 75%;
                background: #f9f9f9;
                border-left: 2px solid #ccc;
                overflow-y: auto;
                padding: 12px;
                font-family: Arial, sans-serif;
                box-shadow: -3px 0 8px rgba(0,0,0,0.1);
                z-index: 9998;
            ">
            <h2 style="margin-top:0;">Package Info</h2>
            <p id="package-name" style="font-weight:bold;"></p>
            <ul id="function-list" style="list-style-type:none; padding-left:0;"></ul>
            </div>
        """
        with open(filename, "a", encoding="utf-8") as f:
            f.write(package_info_panel)      

    add_layer_slider(filename, "src/static/graph_interact.js", max_depth)
    add_hint(filename)
    add_info_panel(filename)