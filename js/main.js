window.addEventListener('load', () => {
    writeCopyrightYear()
    numberSourceCodes()
})

function writeCopyrightYear() {
    let baseYear = '2012'
    let year = new Date().getFullYear().toString()
    let copyrightDate = baseYear === year ? baseYear : `${baseYear}-${year}`
    Array.from(document.querySelectorAll(".copyright-date")).forEach(node => {
        node.innerText = copyrightDate
    })
}

function numberSourceCodes() {
    let lineTextTemplate = document.createElement("span")
    lineTextTemplate.classList.add("line-content")

    Array.from(document.querySelectorAll(".sourceCode")).forEach(sourceCode => {
        let counter = 1
        Array.from(sourceCode.querySelectorAll(":scope > span")).forEach(line => {
            line.classList.add("line")

            let lineNumber = line.childNodes[0]
            lineNumber.classList.add("line-number")
            lineNumber.innerText = counter
            counter++

            let textNodes = [];
            for (let i = 1; i < line.childNodes.length; i++) {
                textNodes.push(line.childNodes[i])
            }
            for (let i = 1; i < line.childNodes.length; i++) {
                line.removeChild(textNodes[i - 1])
            }

            let lineText = lineTextTemplate.cloneNode(true)
            textNodes.forEach(node => lineText.appendChild(node))
            line.appendChild(lineText)
        })
    })
}
