
import { Elm } from './Main.elm';
import bigInt from 'big-integer';

// TODO: add jshashes explicit dependency

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

    hasher.setUTF8(false).setUpperCase(false);

    cachedHashers.set(name, hasher);

    return hasher;
}


// TODO: send PR to elm asking them to change var usages to const in their docs
// TODO: do the same for jshashes

app.ports.askHash.subscribe(request => {
    const [algorithm, input] = request;

    const hasher = getHasher(algorithm);

    if (hasher !== undefined) {
        console.log('asking hash for', input);

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

app.ports.askHashFileContent.subscribe(chageEvent => {
    // event is the result of the onchange event of an input element

    const files = chageEvent.target.files;

    if (!files.length) {
        console.warn('no file detected in input');
        return;
    }

    const file = files[0];

    const reader = new FileReader();

    reader.addEventListener('load', (event) => {
        console.log('onloaded!');

        if (event.target.readyState !== FileReader.DONE) {
            return;
        }

        const result = event.target.result;

        app.ports.onHashFileContent.send(result);

        console.log('done')
        console.log(result);

    });

    reader.addEventListener('error', (event) => {
        console.log('error reading file');
        // TODO: send error to ports
    });

    reader.addEventListener('progress', (event) => {
        console.log('.');
    });

    reader.readAsBinaryString(file);
    console.log('tryna read za file', file);

});

function isPrime(value) {
    return bigInt(value).isProbablePrime(256)
}

app.ports.askIsPrime.subscribe(value => {
    console.log(`elm wants to know if ${value} is prime`);


    const result = isPrime(value);

    app.ports.onPrimalityResolved.send(result);
});
