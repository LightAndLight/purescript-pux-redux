exports.unsafeRestrict = function(isSymbolDict) {
    return function(rowConsDict) {
        return function(proxy) {
            return function(obj) {
                var newObj = Object.assign({}, obj);
                delete newObj[isSymbolDict.reflectSymbol(proxy)];
                return newObj;
            };
        };
    };
};

exports.unsafeExtend = function(isSymbolDict) {
    return function(rowConsDict) {
        return function(proxy) {
            return function(value) {
                return function(obj) {
                    var newObj = {};
                    newObj[isSymbolDict.reflectSymbol(proxy)] = value;

                    return Object.assign({}, obj, newObj);
                };
            };
        };
    };
};

