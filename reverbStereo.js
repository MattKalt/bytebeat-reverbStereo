//Original t, increments one per sample. The reverb, harmonifier, hihat, and snare need this.
T = t,

t *= r8 = 10 / 48,

//seq = ( arr, spd, t2=t ) => arr[ (t2 >> spd) % arr.length ],
/*version that lerps:
	the 'x' argument controls the speed at which the slides happen (1=very slidy, 99=almost none, 0=none) */
seq=(r,s,t2=t,x=0)=>(i=t2/2**s,J=i|0,L=r.length,x?(k=(i-J)**x,(1-k)*r[J%L]+k*r[(J+1)%L]):r[J%L]),

//----SONG SETTINGS-----

//master pitch
mp = -4,



//-----TOOLS-----

/*
	Repeat x beats of y
	SUPER useful if you're writing complex beats/melodies
	Include this or the Fs won't work (or you could replace r(x, y) with Array(x).fill(y))
	r(1,[arrays]) also serves as a replacement for [arrays].flat()
*/
r = repeat = (x, y) => Array( x ).fill( y ).flat( 9 ),

sp = (str, sep='') => str.split( sep ),
j = (arr, sep='') => arr.join( sep ),

//tra = transpose = (arr, amt) => arr.map(x=>x+amt),
tra = transpose = (x, amt) => Array.isArray(x)? x.map( e => e + amt ) : j( sp(x).map( e => e + amt ) ),

//pretty much deprecated but used in bt()
m = mix = (x, vol=1, dist=0) => ( ( x * vol * ( 1 + dist ) ) % ( 256 * vol ) ) || 0,

/*
	F is the FX stack, stores memory for use in effects
	Automatically keeps track of what's stored where
	If you see red (NaNs), raise 2e3 higher, or adjust your reverbs' 'dsp' variable (and limiters' lookahead)
	Works best when effects are not inside conditionals (meaning the number of F in use changes)
	But even then, should only create a momentary click/pop (might be more severe for reverb)
*/
T ? 0 : F = r( 2e3, 0 ),
// Index of F, resets to 0 at every t
I = 0,

//melodic sequences without clicks/pops
//mseq = ( ...x ) => t * 2 ** ( seq(...x) / 12 ), //original
mseq = ( ...x ) => (
	F[I++] += ( r8 * 2 ** ( ( seq(...x) + mp ) / 12))||0
),



/* The Breakbeat drum machine. This is where the magic happens
It sequences through an array and plays the corresponding number of beats
	(1 = quarter note, 2 = 2 8th notes, etc)
Something interesting happens when you don't use powers of 2, however:
	You get strange and wonderful sounds
the variables 's' and 'h' make it sound like a snare and a hihat, respectively
most sounds are different timbres of the same note
but if you replace 't2' with something other than t, such as any bytebeat melody,
you can apply that timbre to the melody.
Adding / &ing a breakbeat with a melody can also add attack to the notes of the melody
*/
bt = beat = (arr, spd, vel = 2e4, vol = 1, t2 = t, oct = 0) =>
	m(vel / (t2 & (2 ** (spd - oct) / seq( arr, spd ) ) - 1), vol),

ls = sin(T / 9 & T >> 5), // long snare
//s = sin(t>>5), // acoustic-sounding grungy snare
s = seq( [ls, 0], 9), // Snare
h = 1 & T * 441/480, // long Hihat
h = seq( [h,h,h,0], 8), //quieter, faster attack

/*
	Stereo delay with multiple heads and a chorusey effect
	inspired by Feeshbread's Dead Data echo,
	but using the GAv2 reverb's downsampling
	(and some messy magic numbers to keep gain in line, save memory and chars, etc)
	single input, outputs an array size 2
	requires old lp(), new hp(), lim2(), r(), and slidy seq() to function
*/

rvs = reverbStereo = (
	input,
	len = 16e3,
	vibratoSpeed = [91,83,77,67,5], //must be 2 or larger
	dry = .4,
	wet = .6,
	feedb =.6, //best results between .6-.86
	downsamp = 3, //can be any float over 1
	lerpx=4, //0 = no interpolation, 1=linear, 2=quadratic, etc.
	highpass=.1,
	lowpass = .7,
	compAtk = 9,
	compRel = 1,
	compThresh = 9, //~9 for bytebeat range (0-255), adjust accordingly for smaller ranges
	vibratoDepth = 299, //best around 300, lower=feedbackier
	voices = vibratoSpeed.length,
	t2 = r(voices, T ), //array of all T the same size as vibratoSpeed[], could also be T/2 if specified in args
) => (
	x = y => I + voices*3 + ( (y % len) / downsamp )|0, //index within allocated fx memory
	fbh=[], out=[0,0], //can reuse fbh to save chars, but first 2 outputs will be double volume
	t2.map( (t2val,i)=> (
		t2val += vibratoDepth + vibratoDepth * sin(T*vibratoSpeed[i]/3e6),
		fbh[i] = hp( lp( seq( F, 0, x(t2val) - i*2, lerpx )||0 , lowpass), highpass)
	)),
	F[ x(T) ] = input * (1-feedb) + fbh.reduce((a,e,i)=> a=lim2(
				a+e, compAtk, compRel/voices, compThresh/voices*(1+i/2)
		) * feedb )
	,
	I += 0|(len / downsamp) + voices*3,
	//fbh.map((e,i)=>fbh[i%2]+=e*wet+input*dry/voices) //first 2 voices will be double volume
	fbh.map((e,i)=>out[i%2]+=e*wet+input*dry/voices),out
),


//bad lopass (turns things into triangles rather than sins) but good for compressor
lp2 = lopass = (input, freq, bias=1) => // f ~= frequency, but not 1:1
	// F[I] is the value of the last sample
	// You will need to change the 'x % 256' if you're using signed or floatbeat
	F[I] = min( max( input, F[I] - freq), F[I++] + freq * bias)||0 // Clamp the change since last sample between (-f, f)
,

//better lopass, especially for hi-pass
lp = (input,freq) =>
	F[I] = F[I++] * (1-freq) + input * freq
,

hp = (input,freq) => input - lp(input,freq),

//simple but bad limiter, uses the bad lopass
//release must be >0
lim2 = (input, atk, release, thresh) => (
	input * thresh / lp2(
		max( thresh, abs(input))
	,release, atk/release
	)||0
),



//saw 2 sine
s2s = sinify = x => sin( x*PI/64 ) * 126 + 128,

t||(

ml1='64835582659355636583658265826421',

bs1 = [-4,1,-4,-7,-9,-11,-2,3],

//vibSpeeds = [91,7],
//vibSpeeds = [91,51,23,7],
//vibSpeeds = [91,83,77,67,7,5,3,2],
//vibSpeeds = r(16,299).map((e,i)=>e/1.618**(i/2)),
vibSpeeds = r(8,199).map((e,i)=>e/1.618**i),

0

),


BS = s2s(mseq(bs1,15)),
//BS *= min(bt([1],12)/25,2),
BS *= lp(min(bt([1],12,20,60),4),.1),
BS *= 64/max(64, abs(BS)),

mel=ml1[t>>11&31]*t%100*(1-t%2048/2048),
chords=(mseq(bs1,15)&63)/4,

V=rvs( mel + chords, 11e3, vibSpeeds, .25, .5, .8 - cos(T/3e5)/16, 6, 0, .1, .5, 9, 1, 9, 299 ),

//V=rvs( mel + chords, 7e3, vibSpeeds, .2, .8, .9, 4, 2, .1, .5, 9, 1, 9, 199 + cos(T/3e5)*99, 8,[T,T*3/4,T/2,T*3/2,T/2,T*2,T/2,T] ), //trippy octaving




Master=ch=>tanh(
	hp(
		V[ch] * 6 + BS * min(2,T/5e5)
		//mel&255
	,.001)
/max(64,128-T/5e4))*1.2,

[Master(0),Master(1)]



//,a=()=>{throw(I)},a() //Determine size of memory stack to initialize
