local linkedTable = require("linked-table")

local function solveOverlapping(results)
    table.sort(results, function(a, b)
        if a.start == b.start then
            return a.finish < b.finish
        else
            return a.start < b.start
        end
    end)

    local tokens = linkedTable()

    local function findToken(pos)
        for token in tokens:pairs(nil, true) do
            if token.start <= pos and token.finish >= pos then return token
            end
            if token.finish < pos then
                break
            end
        end
        return nil
    end

    local i = 0
    for _, current in ipairs(results) do
        print("-----", i, "-----")

        local left = findToken(current.start)
        if not left then
            tokens:pushTail(current)
            print("pushTail")
            for token in tokens:pairs() do
                print(vim.inspect(token))
            end
        else
            local right = findToken(current.finish)
            tokens:pushAfter(current, left)
            print("current pushAfter")
            for token in tokens:pairs() do
                print(vim.inspect(token))
            end
            tokens:pop(left)
            print("pop")
            for token in tokens:pairs() do
                print(vim.inspect(token))
            end
            if left.start < current.start then
                tokens:pushBefore({
                    start = left.start,
                    finish = current.start,
                    type = left.type,
                    modifieres = left.modifieres,
                }, current)
                print("pushBefore")
                for token in tokens:pairs() do
                    print(vim.inspect(token))
                end
            end

            if right and right.finish > current.finish then
                tokens:pushAfter({
                    start = current.finish,
                    finish = right.finish,
                    type = right.type,
                    modifieres = right.modifieres,
                }, current)
                print("right pushAfter")
                for token in tokens:pairs() do
                    print(vim.inspect(token))
                end
            end
        end

        i = i + 1
    end

    local new = {}
    for token in tokens:pairs() do
        new[#new + 1] = token
    end

    return new
end

-- [[
--
-- Hello.Hello.World - (0, 17)
--      .            - (5, 5)
--            .      - (11, 11)
--
-- ]]

local function token(array_table)
    return {
        start = array_table[1],
        finish = array_table[2],
        type = array_table[3],
        modifieres = array_table[4]
    }
end

local results = {
    token({ 0, 17, "Namespace", 1 }),
    token({ 5, 5, "Operator", 0 }),
    token({ 11, 11, "Operator", 0 }),
}

print(vim.inspect(solveOverlapping(results)))
