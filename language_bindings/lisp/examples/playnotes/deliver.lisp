
(push :deliver *features*)
(load "examples/playnotes/playnotes")

(deliver 'audio-engine::playnotes "playnotes" 1)
