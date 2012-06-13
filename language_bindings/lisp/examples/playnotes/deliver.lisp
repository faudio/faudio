
(push :deliver *features*)
(load "examples/playnotes/playnotes")

(deliver 'audio-engine::main "playnotes" 1)
