
import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('app')
});

const cachedHashers = new Map();

const hashers = {
    'MD5': () => new Hashes.MD5(),
    'SHA1': () => new Hashes.SHA1(),
    'SHA256': () => new Hashes.SHA256(),
    'MD5': () => new Hashes.MD5(),
};

function getHasher(name) {
    const cached = cachedHashers.get(name);

    if (cached !== undefined) {
        return cached;
    }

    const hasher = hashers[name]();

    if (hasher === undefined) {
        return undefined;
    }

    hasher.setUTF8(true).setUpperCase(false);

    cachedHashers.set(name, hasher);

    return hasher;
}


// TODO: send PR to elm asking them to change var usages to const in their docs
// TODO: do the same for jshashes

app.ports.askHash.subscribe(request => {
    const [algorithm, input] = request;

    const hasher = getHasher(algorithm);

    if (hasher !== undefined) {
        const result = hasher.hex(input);

        app.ports.onHash.send(result);
    }
});

app.ports.askHmac.subscribe(request => {
    const {algorithm, message, key} = request;

    const hasher = getHasher(algorithm);

    if (hasher !== undefined) {
        const result = hasher.hex_hmac(key, message);

        app.ports.onHmac.send(result);
    }
});
