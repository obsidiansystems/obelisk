/*
  :: [String] -> Map String Derivation

  Given `pkgs`, a set of package names, compute the set of packages which are both dependencies and dependents of `pkgs`.

  Example:
  ```
    $ echo 'sandwichedDeps' | nix repl lib.nix --arg pkgs '["backend" "common" "frontend" "reflex-dom-core"]'

    { obelisk-backend = «derivation /nix/store/xbdrjxs0mvs9hjs1c30njvqbc6839p4s-obelisk-backend-0.1.drv»
    ; obelisk-executable-config-inject = «derivation /nix/store/9y1w40d1lr7s6m7jv4z6aij3n17wqasb-obelisk-executable-config-inject-0.1.drv»
    ; obelisk-executable-config-lookup = «derivation /nix/store/86d87qfhn3vzsgn5v44n4hc763xp1lx3-obelisk-executable-config-lookup-0.1.1.drv»
    ; obelisk-frontend = «derivation /nix/store/ljh0wzcqmpwn2faxfflpl2nfiialhjwa-obelisk-frontend-0.1.drv»
    ; obelisk-route = «derivation /nix/store/nzxaai6s1hxrz6lb9cn0hn24x0pzvp3y-obelisk-route-0.2.drv»;
    }
  ```
*/

{ pkgs
}: with builtins;
let
  proj = import ./default.nix {};
in
  with proj.obelisk.nixpkgs.lib; rec {
    # Why are boot packages null?
    cabalDeps = p: filter (p: p != null) p.getCabalDeps.libraryHaskellDepends;
    depAttrs = p: listToAttrs (map (d: nameValuePair (d.pname) d) (cabalDeps p));

    node = mkEdges: p: { ${p.pname} = { drv = p; edges = mkEdges p; };};
    children = mkEdges: nodes: concatMap (n: map (node mkEdges) (attrValues n.edges)) (attrValues nodes);
    expand = mkEdges: nodes: foldl' (x: y: x // y) nodes (children mkEdges nodes);

    closureCabalDeps = converge (expand depAttrs);
    closureEdges = graph: converge (expand (p: graph.${p.pname}.edges));

    dual = nodes:
      let
        edges = concatMap (n: map (m: { ${m.pname} = [n]; }) (attrValues n.edges)) (attrValues nodes);
        adjacencies = zipAttrsWith (name: concatLists) edges;
        mkNode = k: v: { drv = nodes.${k}.drv; edges = listToAttrs (map (d: nameValuePair d.drv.pname d.drv) v); };
        preserveNodesWithoutDependents = ns: mapAttrs (k: v: v // {edges = {};}) nodes // ns;
      in
        preserveNodesWithoutDependents (mapAttrs mkNode adjacencies);

    merge = foldl' (x: y: x // y) {};

    sandwichedDeps =
      let
        g = closureCabalDeps (merge (map (node depAttrs) (map (p: proj.ghc.${p}) pkgs)));
        d = dual g;
        dependents = closureEdges d (merge (map (p: { ${p} = d.${p}; }) pkgs));
      in
        mapAttrs (k: v: v.drv) (removeAttrs dependents pkgs);
  }
