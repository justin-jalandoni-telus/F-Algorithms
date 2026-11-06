namespace Algorithms

/// Maximum matching in a general (not necessarily bipartite) graph via Edmonds'
/// blossom algorithm, in O(V^3). The algorithm is inherently imperative, so it
/// uses isolated internal mutation (arrays indexed by vertex) behind a pure
/// interface: the graph goes in, a list of matched pairs comes out.
[<RequireQualifiedAccess>]
module GeneralMatching =

    /// The maximum matching as a list of vertex pairs (each edge once).
    let maximumMatching (graph: Graph) : (Vertex * Vertex) list =
        let vertices = Graph.vertices graph |> List.toArray
        let n = vertices.Length
        let index = vertices |> Array.mapi (fun i v -> v, i) |> Map.ofArray

        let adjacency =
            Array.init n (fun i ->
                Graph.neighbours vertices.[i] graph
                |> List.map (fun (t, _) -> index.[t])
                |> List.toArray)

        let matchTo = Array.create n -1
        let parent = Array.create n -1
        let baseOf = Array.init n id
        let used = Array.create n false
        let inBlossom = Array.create n false
        let queue = System.Collections.Generic.Queue<int>()

        let lowestCommonAncestor a b =
            let seen = Array.create n false
            let mutable x = a
            let mutable finished = false

            while not finished do
                x <- baseOf.[x]
                seen.[x] <- true
                if matchTo.[x] = -1 then finished <- true else x <- parent.[matchTo.[x]]

            let mutable y = b
            let mutable ancestor = -1

            while ancestor = -1 do
                y <- baseOf.[y]
                if seen.[y] then ancestor <- y else y <- parent.[matchTo.[y]]

            ancestor

        let markBlossom ancestor child start =
            let mutable v = start
            let mutable c = child

            while baseOf.[v] <> ancestor do
                inBlossom.[baseOf.[v]] <- true
                inBlossom.[baseOf.[matchTo.[v]]] <- true
                parent.[v] <- c
                c <- matchTo.[v]
                v <- parent.[matchTo.[v]]

        let contractBlossom v w =
            let ancestor = lowestCommonAncestor v w
            Array.fill inBlossom 0 n false
            markBlossom ancestor w v
            markBlossom ancestor v w

            for i in 0 .. n - 1 do
                if inBlossom.[baseOf.[i]] then
                    baseOf.[i] <- ancestor

                    if not used.[i] then
                        used.[i] <- true
                        queue.Enqueue i

        let findAugmentingPath root =
            Array.fill used 0 n false
            Array.fill parent 0 n -1
            for i in 0 .. n - 1 do
                baseOf.[i] <- i

            used.[root] <- true
            queue.Clear()
            queue.Enqueue root
            let mutable endpoint = -1

            while endpoint = -1 && queue.Count > 0 do
                let v = queue.Dequeue()
                let mutable j = 0

                while endpoint = -1 && j < adjacency.[v].Length do
                    let w = adjacency.[v].[j]
                    j <- j + 1

                    if baseOf.[v] = baseOf.[w] || matchTo.[v] = w then
                        ()
                    elif w = root || (matchTo.[w] <> -1 && parent.[matchTo.[w]] <> -1) then
                        contractBlossom v w
                    elif parent.[w] = -1 then
                        parent.[w] <- v

                        if matchTo.[w] = -1 then
                            endpoint <- w
                        else
                            used.[matchTo.[w]] <- true
                            queue.Enqueue matchTo.[w]

            endpoint

        for root in 0 .. n - 1 do
            if matchTo.[root] = -1 then
                let endpoint = findAugmentingPath root

                if endpoint <> -1 then
                    let mutable u = endpoint

                    while u <> -1 do
                        let pv = parent.[u]
                        let ppv = matchTo.[pv]
                        matchTo.[u] <- pv
                        matchTo.[pv] <- u
                        u <- ppv

        [ for i in 0 .. n - 1 do
              if matchTo.[i] > i then
                  yield vertices.[i], vertices.[matchTo.[i]] ]
