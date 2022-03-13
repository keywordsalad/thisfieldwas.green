window.addEventListener("load", () => {
    writeCopyrightYear()
})

function writeCopyrightYear() {
    let baseYear = "2012"
    let year = new Date().getFullYear().toString()
    let copyrightDate = baseYear === year ? baseYear : `${baseYear}-${year}`
    Array.from(document.querySelectorAll(".copyright-date")).forEach(node => {
        node.innerText = copyrightDate
    })
}
