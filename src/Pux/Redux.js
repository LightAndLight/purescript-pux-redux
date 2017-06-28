"use strict";

exports.merge = function (dict) {
    return function (l) {
        return function (r) {
            return Object.assign({}, r, l);
        };
    };
};

exports.removeField = function(isSymbolDict) {
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

exports.addField = function(isSymbolDict) {
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
