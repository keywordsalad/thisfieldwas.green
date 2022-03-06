window.addEventListener("load", () => {
    writeCopyrightYear()
    addCodeScrolling()
})

function writeCopyrightYear() {
    let baseYear = "2012"
    let year = new Date().getFullYear().toString()
    let copyrightDate = baseYear === year ? baseYear : `${baseYear}-${year}`
    Array.from(document.querySelectorAll(".copyright-date")).forEach(node => {
        node.innerText = copyrightDate
    })
}

function addCodeScrolling() {
    document.querySelectorAll("div.sourceCode").forEach(wrapper => {
        let content = wrapper.querySelector("pre.sourceCode")
        let contentScrollWidth = content.scrollWidth - wrapper.offsetWidth
        if (contentScrollWidth <= 0) {
            return;
        }

        let leftShadow = document.createElement("div")
        leftShadow.classList.add("shadow", "shadow-left")
        wrapper.appendChild(leftShadow)

        let rightShadow = document.createElement("div")
        rightShadow.classList.add("shadow", "shadow-right")
        wrapper.appendChild(rightShadow)

        function setScroll() {
            let currentScroll = content.scrollLeft / contentScrollWidth
            leftShadow.style.opacity = currentScroll
            rightShadow.style.opacity = 1 - currentScroll
        }
        setScroll();
        content.addEventListener("scroll", setScroll);
    })
}
