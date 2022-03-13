function updateScrollShadowsXSelector(selector) {
  document.querySelectorAll(selector).forEach(updateScrollShadowsX)
}

function updateScrollShadowsYSelector(selector) {
  document.querySelectorAll(selector).forEach(updateScrollShadowsY)
}

function updateScrollShadowsX(wrapper) {
  // identify the scrolling content within the wrapper
  let contentQuery = wrapper.querySelector(".scroll-content")
  let content
  if (contentQuery) {
    content = contentQuery
  } else {
    content = wrapper.children[0]
    content.classList.add("scroll-content")
  }

  let contentScrollWidth = content.scrollWidth - wrapper.offsetWidth
  // remove shadows if no scrolling is needed
  if (contentScrollWidth <= 0) {
    wrapper.querySelectorAll(".shadow-x").forEach(x => x.remove())
    delete wrapper.updateScrollShadowsX
    return
  }

  // skip if scroll shadows already added
  if (wrapper.updateScrollShadowsX) {
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
      shadow.classList.add("shadow-x", `shadow-${side}`)
      wrapper.appendChild(shadow)
    }
    return shadow
  })
  // add method to update shadow opacity relative to scroll
  wrapper.updateScrollShadowsX = () => {
    let currentScroll = content.scrollLeft / contentScrollWidth
    leftShadow.style.opacity = currentScroll
    rightShadow.style.opacity = 1 - currentScroll
  }
  // attach event listener to update shadows
  content.addEventListener("scroll", function () {
    if (wrapper.updateScrollShadowsX) {
      wrapper.updateScrollShadowsX()
    }
  })
  wrapper.updateScrollShadowsX()
}

function updateScrollShadowsY(wrapper) {
  // identify the scrolling content within the wrapper
  let contentQuery = wrapper.querySelector(".scroll-content")
  let content
  if (contentQuery) {
    content = contentQuery
  } else {
    content = wrapper.children[0]
    content.classList.add("scroll-content")
  }

  let contentScrollHeight = content.scrollHeight - wrapper.offsetHeight
  // remove shadows if no scrolling is needed
  if (contentScrollHeight <= 0) {
    wrapper.querySelectorAll(".shadow-y").forEach(x => x.remove())
    delete wrapper.updateScrollShadowsY
    return
  }

  // skip if scroll shadows already added
  if (wrapper.updateScrollShadowsY) {
    return;
  }

  // create scroll shadows
  let [topShadow, bottomShadow] = ["top", "bottom"].map(side => {
    let shadowQuery = wrapper.querySelector(`.shadow-${side}`)
    let shadow
    if (shadowQuery) {
      shadow = shadowQuery
    } else {
      shadow = document.createElement("div")
      shadow.classList.add("shadow-y", `shadow-${side}`)
      wrapper.appendChild(shadow)
    }
    return shadow
  })
  // add method to update shadow opacity relative to scroll
  wrapper.updateScrollShadowsY = () => {
    let currentScroll = content.scrollTop / contentScrollHeight
    topShadow.style.opacity = currentScroll
    bottomShadow.style.opacity = 1 - currentScroll
  }
  // attach event listener to update shadows
  content.addEventListener("scroll", function () {
    if (wrapper.updateScrollShadowsY) {
      wrapper.updateScrollShadowsY()
    }
  });
  wrapper.updateScrollShadowsY()
}
