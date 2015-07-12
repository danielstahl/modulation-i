
//Snävare bandvåg = mer ton (glas/metall)
(
Synth("pulseInstrument", [pulseFreq: 0.34, filtFreq: 70, filtBw: 0.00001, amp: 1, dur: 6 ]);
Synth("pulseInstrument", [pulseFreq: 0.13, filtFreq: 99, filtBw: 0.00001, amp: 1, dur: 6 ]);
)

(
Synth("pulseInstrument", [startPulseFreq: 5, endPulseFreq: 23, startFiltFreq: 7000, endFiltFreq: 9000, startFiltBw: 0.0000001, endFiltBw: 0.00001, amp: 1, dur: 13, startPan: -1, endPan: 1 ]);
Synth("pulseInstrument", [startPulseFreq: 7, endPulseFreq: 25, startFiltFreq: 6000, endFiltFreq: 8500, startFiltBw: 0.00001, endFiltBw: 0.0000001, amp: 1, dur: 13 ]);
Synth("noiseInstrument", [startFiltFreq: 200, endFiltFreq: 1000, startFiltBw: 0.001, endFiltBw: 0.0001, amp: 0.9, dur: 13, startPan: -1, endPan: 1 ]);
)

(
Synth("noiseInstrument", [startFiltFreq: 200, endFiltFreq: 1000, startFiltBw: 0.001, endFiltBw: 0.0001, amp: 0.9, dur: 13, startPan: -1, endPan: 1 ]);
)


/*
http://community.dur.ac.uk/nick.collins/teaching/supercollider/sctutorial/6.4 Effects 1.html
http://ecmc.rochester.edu/ecmc/docs/supercollider/scbook/Ch21_Interface_Investigations/ixi SC tutorial/ixi_SC_tutorial_12.html

http://doc.sccode.org/Tutorials/Mark_Polishook_tutorial/17_Delays_reverbs.html
http://doc.sccode.org/Classes/NestedAllpassC.html

(
{
var s, z, y;
    // 10 voices of a random sine percussion sound :
s = Mix.ar(Array.fill(10, { Resonz.ar(Dust.ar(0.2, 50), 200 + 3000.0.rand, 0.003)}) );
    // reverb predelay time :
z = DelayN.ar(s, 0.048);
    // 7 length modulated comb delays in parallel :
y = Mix.ar(Array.fill(7,{ CombL.ar(z, 0.1, LFNoise1.kr(0.1.rand, 0.04, 0.05), 15) }));
    // two parallel chains of 4 allpass delays (8 total) :
4.do({ y = AllpassN.ar(y, 0.050, [0.050.rand, 0.050.rand], 1) });
    // add original sound to reverb and play it :
s+(0.2*y)
}.scope
)
*/



/*
Maybee we should use Rotate2 to rotate sounds?
(
{
    // rotation via lfo
    var x, y;
    x = PinkNoise.ar(0.4);
    y = LFTri.ar(800) * LFPulse.kr(3,0,0.3,0.2);
    #x, y = Rotate2.ar(x, y, LFSaw.kr(0.1));
    [x,y]
}.play;
)

*/

/*
Make delay with "composed" speed and delaytime from series

*/