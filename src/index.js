import { Elm } from "./game/Main.elm";
import * as webaudio from './lib/WebAudio.js'

const app = Elm.Main.init({ node: document.getElementById("root") });
webaudio.setup(app);
