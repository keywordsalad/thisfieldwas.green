let sideScrollingSelectors = ["pre.sourceCode"]

window.addEventListener("load", () => {
    writeCopyrightYear()
    sideScrollingSelectors.forEach(updateScrollShadowsSelector)
})

window.addEventListener("resize", () => {
    sideScrollingSelectors.forEach(updateScrollShadowsSelector)
})

function writeCopyrightYear() {
    let baseYear = "2012"
    let year = new Date().getFullYear().toString()
    let copyrightDate = baseYear === year ? baseYear : `${baseYear}-${year}`
    Array.from(document.querySelectorAll(".copyright-date")).forEach(node => {
        node.innerText = copyrightDate
    })
}

function updateScrollShadowsSelector(selector) {
    document.querySelectorAll(selector).forEach(updateScrollShadows)
}

function updateScrollShadows(wrapper) {
    // identity the scrolling content within the wrapper
    let contentQuery = wrapper.querySelector(".scroll-content")
    let content
    if (contentQuery) {
        content = contentQuery
    } else {
        content = wrapper.firstChild
        content.classList.add("scroll-content")
    }

    let contentScrollWidth = content.scrollWidth - wrapper.offsetWidth
    // remove shadows if no scrolling is needed
    if (contentScrollWidth <= 0) {
        wrapper.querySelectorAll(".shadow").forEach(x => x.remove())
        delete wrapper.updateScrollShadows
        return
    }

    // skip if scroll shadows already added
    if (wrapper.updateScrollShadows) {
        return;
    }

    // create scroll shadows
    let [leftShadow, rightShadow] = ["left", "right"].map(side => {
        let shadowQuery = wrapper.querySelector(`.shadow-${side}`)
        let shadow
        if (shadowQuery) {
            shadow = shadowQuery
        } else {
            shadow = document.createElement("div")
            shadow.classList.add("shadow", `shadow-${side}`)
            wrapper.appendChild(shadow)
        }
        return shadow
    })
    // add method to update shadow opacity relative to scroll
    wrapper.updateScrollShadows = () => {
        let currentScroll = content.scrollLeft / contentScrollWidth
        leftShadow.style.opacity = currentScroll
        rightShadow.style.opacity = 1 - currentScroll
    }
    // attach event listener to update shadows
    content.addEventListener("scroll", function () {
        if (wrapper.updateScrollShadows) {
            wrapper.updateScrollShadows()
        }
    })
    wrapper.updateScrollShadows()
}
