function pureNull(aOrNull) {
  return aOrNull
}

function isNull(aOrNull) {
  return (aOrNull == null) || (typeof aOrNull == 'undefined');
}

function foldNull(z) {
  return function(aOrNull) {
    return isNull(aOrNull) ? z : aOrNull;
  }
}

function mapNull(f) {
  return function(aOrNull) {
    return isNull(aOrNull) ? null : f(aOrNull);
  }
}

function showNull(aOrNull) {
  return isNull(aOrNull) ? 'naught' : aOrNull.toString();
}

function unsafeUnNull(aOrNull) {
  return aOrNull
}

module.exports = {
  'null': null,
  pureNull: pureNull,
  isNull: isNull,
  foldNull: foldNull,
  mapNull: mapNull,
  showNull: showNull,
  unsafeUnNull: unsafeUnNull
}
