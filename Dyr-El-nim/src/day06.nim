import tables
import strutils

type
    OrbitMap = Table[string, string]
    ObjOrbits = tuple[direct: int, indirect: int]
    Orbits = Table[string, ObjOrbits]
    OrbitTrans = Table[string, int]

proc parseOrbits(desc: string): OrbitMap =
    for s in desc.splitLines:
        let
            token = s.strip
            pos = token.find(")")
            baseObj = token.substr(0, pos-1)
            orbObj = token.substr(pos+1, len(token))
        result[orbObj] = baseObj

proc calcOrbit(orbMap: OrbitMap, orbits: var Orbits, obj: string) =
    if orbits.hasKey(obj):
        return
    if not orbMap.hasKey(obj):
        orbits[obj] = (direct:0, indirect:0)
    else:
        let
            baseObj = orbMap[obj]
        calcOrbit(orbMap, orbits, baseObj)
        let
            baseOrbit = orbits[baseObj]
        orbits[obj] = (direct: 1, 
                       indirect: baseOrbit.direct + baseOrbit.indirect)

proc calcOrbits(orbMap: OrbitMap): Orbits =
    for key in orbMap.keys:
        calcOrbit(orbMap, result, key)

proc totalOrbits*(desc: string): int =
    let
        orbitMap = parseOrbits(desc)
        orbits = calcOrbits(orbitMap)
    for obj in orbits.keys:
        let
            objOrbits = orbits[obj]
        result.inc(objOrbits.direct + objOrbits.indirect)

proc calcOrbitTrans(orbMap: OrbitMap, obj: string): OrbitTrans =
    var
        noTransfers = 0
        currentObj = obj
    while orbMap.hasKey(currentObj):
        currentObj = orbMap[currentObj]
        result[currentObj] = noTransfers
        noTransfers.inc

proc minOrbitTrans*(desc: string, src:string, dst: string): int =
    let
        orbitMap = parseOrbits(desc)
        orbTrans = calcOrbitTrans(orbitMap, src)
    var
        noTransfers = 0
        currentObj = dst
    while orbitMap.hasKey(currentObj):
        currentObj = orbitMap[currentObj]
        if orbTrans.hasKey(currentObj):
            break
        noTransfers.inc
    return noTransfers + orbTrans[currentObj]
