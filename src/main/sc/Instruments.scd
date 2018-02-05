
/* Moduler instruments */
(

SynthDef(\lineControl, {
	arg dur = 1, startValue = 1, endValue = 1, out = 0;
	var lineEnv;
	lineEnv = Line.kr(start: startValue, end: endValue, dur: dur, doneAction:2);
	Out.kr(out, lineEnv);
}).add;

SynthDef(\asrControl, {
	arg dur = 1, out = 0,
	attackStart, sustainStart, decayStart, decayEnd,
	attackTime, sustainTime, decayTime;
	var asrEnv;
	asrEnv = Env(
		levels:[attackStart, sustainStart, decayStart, decayEnd],
		times: [attackTime, sustainTime, decayTime],
		curve: \sin);
	asrEnv.duration = dur;
	Out.kr(out, EnvGen.kr(asrEnv, doneAction:2));
}).add;

SynthDef(\arControl, {
	arg dur = 1, out = 0,
	attackStart, releaseStart, releaseEnd,
	attackTime, attackType = \linear, releaseType = \linear;
	var arEnv;
	arEnv = Env(
		levels:[attackStart, releaseStart, releaseEnd],
		times: [attackTime, 1 - attackTime],
		curve: [attackType, releaseType]);
	arEnv.duration = dur;
	Out.kr(out, EnvGen.kr(arEnv, doneAction:2));
}).add;

SynthDef(\sineControl, {
	arg dur, out = 0, phase = 0,
	startFreq, endFreq,
	mulStart = 1, mulEnd = 1,
	addStart = 0, addEnd = 0;
	var freqEnv, mulEnv, addEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	mulEnv = Line.kr(mulStart, mulEnd, dur);
	addEnv = Line.kr(addStart, addEnd, dur);
	Out.kr(out, SinOsc.kr(freq:freqEnv, phase:phase, mul:mulEnv, add:addEnv));
}).add;

SynthDef(\sineControlReplace, {
	arg dur = 1, in = 0, startFreq = 1, endFreq = 1, startAmp = 1, endAmp = 1;
	var freqEnv, input, ampEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	ampEnv = Line.kr(startAmp, endAmp, dur);
	input = In.kr(in);
	ReplaceOut.kr(in, input + SinOsc.kr(freqEnv, 0, ampEnv));
}).add;

SynthDef(\pinkNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	noise = PinkNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\whiteNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	noise = WhiteNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\pulse, {
	arg dur = 1, ampBus = 0, freqBus = 1, widthBus = 2, out = 0;
	var pulse, amp, freq, width;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	width = In.kr(widthBus);
	pulse = Pulse.ar(freq, width);
	Out.ar(out, pulse * amp);
}).add;

SynthDef(\sine, {
	arg dur, freqBus, ampBus, out = 0;
	var freq, amp;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	amp = In.kr(ampBus);

	Out.ar(out, SinOsc.ar(freq) * amp);
}).add;

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

SynthDef(\filt, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReject, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReplace, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\filtRejectReplace, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\highPass, {
	arg dur = 1, freqBus = 1, in = 0, out = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = HPF.ar(input, freq);
	Out.ar(out, filt);
}).add;

SynthDef(\highPassReplace, {
	arg dur = 1, freqBus = 1, in = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = HPF.ar(input, freq);
	ReplaceOut.ar(in, filt);
}).add;

SynthDef(\lowPass, {
	arg dur = 1, freqBus = 1, in = 0, out = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = LPF.ar(input, freq);
	Out.ar(out, filt);
}).add;

SynthDef(\lowPassReplace, {
	arg dur = 1, freqBus = 1, in = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = LPF.ar(input, freq);
	ReplaceOut.ar(in, filt);
}).add;

SynthDef(\limit, {
	arg level = 1, dur = 0.01, in = 0, out = 0;
	var input;
	input = In.ar(in, 1);
	Out.ar(out, Limiter.ar(input, level, dur));
}).add;

SynthDef(\limitReplace, {
	arg level = 1, dur = 0.01, in = 0;
	var input;
	input = In.ar(in, 1);
	ReplaceOut.ar(in, Limiter.ar(input, level, dur));
}).add;

SynthDef(\pan, {
	arg dur = 1, panBus = 0, in = 0, out = 0;
	var pan, input;
	Line.kr(dur:dur, doneAction:2);
	pan = In.kr(panBus);
	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, pan));
}).add;

SynthDef(\stereoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 2);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolumeReplace, {
	arg dur = 1, ampBus = 0, in = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * amp);
}).add;

SynthDef(\monoDelay, {
	arg dur = 1, in = 0, out = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	Out.ar(out, DelayN.ar(input, maxDelay, delay));
}).add;

SynthDef(\monoDelayReplace, {
	arg dur = 1, in = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, DelayN.ar(input, maxDelay, delay));
}).add;

SynthDef(\monoComb, {
	arg dur = 1, in = 0, out = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);
	Out.ar(out, CombL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoCombReplace, {
	arg dur = 1, in = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);

	ReplaceOut.ar(in, CombL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoAllpass, {
	arg dur = 1, in = 0, out = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);
	Out.ar(out, AllpassL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoAllpassReplace, {
	arg dur = 1, in = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);

	ReplaceOut.ar(in, AllpassL.ar(input, maxDelay, delay, decay));
}).add;
)
