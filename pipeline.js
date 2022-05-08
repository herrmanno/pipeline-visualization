function print_node(filepath) {
    const fs = require("fs")

    const file = fs.readFileSync(process.argv[2] || "./build/release/fib3.objdump").toString("ascii")
    const lines = file.split("\n").map(s => s.trim())
    const instructions = lines.flatMap(toInstruction)

    print(instructions)
}

function print_html(str) {
    const lines = str.split("\n").map(s => s.trim())
    const instructions = lines.flatMap(toInstruction)

    return create_tables(instructions)
}

function toInstruction(s) {
    const parts = s.split(/\s/).filter(ss => ss !== "")
    if (!/[0-9a-f]+:/.test(parts[0])) {
        return []
    } else {
        const instr =
            parts.slice(1)
                .filter(p => !/^[0-9a-f]+$/.test(p))
                .filter(p => !/^<.*>$/.test(p))
                .map(s => s.endsWith(',') ? s.slice(0,-1) : s)
        return [[instr[0], instr.slice(1).map(toArguments)]]
    }
}

function toArguments(s) {
    if (s.startsWith('(') && s.endsWith(')')) {
        return { type: "triple", value: s, values: s.slice(1, -1).split(',').map(s => s.trim()).map(toArguments) }
    } else if (s[0] === '$') {
        return { type: "literal", value: s.slice(1) }
    } else if (/^\d+$/.test(s)) {
        return { type: "literal", value: s }
    } else if (s[0] === '%') {
        return { type: "register", value: s.slice(1) }
    } else if (s.slice(0,2) === "0x") {
        return { type: "address", value: s }
    } else if (/^-?(:0x?)?\d+\(.+\)/.test(s)) {
        return { type: "memory address", value: s, register: s.match(/.*\((.*)\)/)[1].slice(1) }
    } else {
        return { type: "unknown", value: s }
    }
}

function is_slow_instruction(i) {
    return (
        /push/.test(i[0]) ||
        /mov/.test(i[0]) ||
        i[1].some(a => a.type === "address" || a.type === "memory address")
    )
}

function arg_values(a) {
    return a.values || [a]
}

/**
 * @deprecated
 */
function value_equals(a, b) {
    if (a.type === "register" && b.type === "register") {
        return a.value === b.value
    } else if (a.type === "register") {
        return a.value === b.register
    } else if (b.type === "register") {
        return a.register === b.value
    } else if (a.register && b.register) {
        return a.register === b.register
    } else {
        return false
    }
}

/**
 * Checks if a depends on b
 */
function depends_on(a, b) {
    if (a.type === "register" && b.type === "register") {
        return a.value === b.value
    } else if (a.type === "memory address" && b.type === "register") {
        return a.register === b.value
    } else {
        return false
    }
}

function print(instrs, max = 20) {
    const width = 3
    let offset = 0
    let lastArgs = []
    for (const instr of instrs) {
        const command = (" ".repeat(6) + instr[0]).slice(-6)
        const args = instr[1]
        const execute =
            is_slow_instruction(instr)
            // (args.some(a => a.type === "memory address") || /mov/.test(command))
            ? " X "
            : " x "
        const wait =
            args.flatMap(arg_values).some(a => lastArgs.flatMap(arg_values).some(b => value_equals(a,b)))
            ? " ".repeat(width)
            : ""

        const argsStr = args.map(a => a.value)
        console.log(" ".repeat(width * offset) + command + " F " + " D " + wait + " R " + execute + " W " + argsStr)
        lastArgs = args.slice(-1) // don't set if command is 'push'
        offset++

        if (offset > max) {
            console.log("")
            offset = 0
        }
    }
}


function create_tables(instrs, max = 20) {
    let table = null
    let out = []

    let offset = 0
    let lastArgs = []

    for (const instr of instrs) {
        if (null === table) {
            table = document.createElement("table")
            out.push(table)
        }

        const tr = document.createElement("tr")

        const command = instr[0]
        const args = instr[1]

        tr.appendChild((() => {
            const td = document.createElement("td")
            const argsStr =
                args
                    .map(a => {
                        const hasDependency = 
                            arg_values(a).some(a => lastArgs.flatMap(arg_values).some(b => depends_on(a,b)))
                        if (hasDependency) {
                            return "<span class='dependency'>" + a.value + "</span>"
                        } else {
                            return a.value
                        }
                    })
                    .join(", ")

            td.innerHTML = command + " " + argsStr
            td.title = command + " " + args.map(a => a.value).join(", ")
            td.classList.add("command")
            return td
        })())

        for (let i = 0; i < offset; i++) {
            tr.appendChild((() => {
                const td = document.createElement("td")
                td.classList.add("offset")
                return td
            })())
        }

        tr.appendChild((() => {
            const td = document.createElement("td")
            td.innerText = "F"
            return td
        })())

        tr.appendChild((() => {
            const td = document.createElement("td")
            td.innerText = "D"
            return td
        })())

        if (args.flatMap(arg_values).some(a => lastArgs.flatMap(arg_values).some(b => depends_on(a,b)))) {
            tr.appendChild((() => {
                const td = document.createElement("td")
                td.classList.add("wait")
                return td
            })())
        }

        tr.appendChild((() => {
            const td = document.createElement("td")
            td.innerText = "R"
            return td
        })())

        tr.appendChild((() => {
            const td = document.createElement("td")
            td.innerText = 'X'
            if (is_slow_instruction(instr)) {
                td.classList.add("slow_instruction")
            }
            return td
        })())

        tr.appendChild((() => {
            const td = document.createElement("td")
            td.innerText = "W"
            return td
        })())

        table.appendChild(tr)

        lastArgs = /push/.test(command) ? [] : args.slice(-1) // don't set if command is 'push'
        offset++

        if (offset > max) {
            table = null
            offset = 0
        }

    }

    return out
}
