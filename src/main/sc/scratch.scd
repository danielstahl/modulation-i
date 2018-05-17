
//"https://composerprogrammer.com/teaching/supercollider/sctutorial/2.4 Modulation Synthesis.html"




play({ PMOsc.ar(Line.kr(600, 900, 5), 600, 3, 0, 0.1) }); // modulate carfreq
play({ PMOsc.ar(300, Line.kr(600, 900, 5), 3, 0, 0.1) }); // modulate modfreq
play({ PMOsc.ar(300, 550, Line.ar(0,20,8), 0, 0.1) }); // modulate index


(
e = Env.linen(2, 5, 2);
Routine.run({
    loop({
        play({
            LinPan2.ar(EnvGen.ar(e) *
                PMOsc.ar(2000.0.rand,800.0.rand, Line.kr(0.0, 12.0.rand,9),0,0.1), 1.0.rand2)});
        2.wait;
    })
}))



(

SynthDef(\fm, {
	arg dur, carFreqBus, modFreqBus, modIndexBus, ampBus, out = 0;
	var carFreq, modFreq, modIndex, amp;
	Line.kr(dur:dur, doneAction:2);
	carFreq = In.kr(carFreqBus);
	modFreq = In.kr(modFreqBus);
	modIndex = In.kr(modIndexBus);
	amp = In.kr(ampBus);

	Out.ar(out, PMOsc.ar(carFreq, modFreq, modIndex, 0, 1, 0) * amp);
}).add;


)

(
Synth("lineControl", [dur: 15, startValue: 500, endValue: 600, out: 13], addAction:\addToTail);
Synth("lineControl", [dur: 15, startValue: 700, endValue: 750, out: 14], addAction:\addToTail);
Synth("arControl", [dur: 15, attackTime: 0.7, attackStart: 1, releaseStart: 10, releaseEnd: 1, out: 15], addAction:\addToTail);
Synth("arControl", [dur: 15, attackTime: 0.3, attackStart: 0, releaseStart: 1, releaseEnd: 0, out: 16], addAction:\addToTail);

Synth("fm", [dur: 15, carFreqBus: 13, modFreqBus: 14, modIndexBus: 15, ampBus: 16, out: 0], addAction:\addToTail);


)

{ Saw.ar(100,0.2) }.play;
{ Pulse.ar(100,0.2) }.play;
{ LFTri.ar(100,0.5) }.play;

{ SinOsc.ar(440+(SinOsc.ar(880)*880*16), 0.0,0.1) }.play;



