export function renderList(filteredFuncs) {
  listEl.innerHTML = "";
  filteredFuncs.forEach((fn) => {
    const li = document.createElement("li");
    li.textContent = fn;
    li.style.borderBottom = "1px solid #ddd";
    li.style.padding = "2px 0";
    li.style.cursor = "pointer";

    // Highlight on click
    li.addEventListener("click", () => {
      listEl.querySelectorAll("li").forEach((el) => {
        el.style.backgroundColor = "";
        el.style.fontWeight = "normal";
      });
      li.style.backgroundColor = "#e0e0e0";
      console.log(`Selected function: ${fn}`);
    });

    listEl.appendChild(li);
  });
}
