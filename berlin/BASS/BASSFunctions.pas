unit BASSFunctions;

interface

Uses
  System.Types, System.SysUtils {$IFDEF MSWINDOWS} , Windows{$ENDIF};

const
  BASSVERSION = $204; // API version
  BASSVERSIONTEXT = '2.4';

  // Use these to test for error from functions that return a DWORD or QWORD
  DW_ERROR = LongWord(-1); // -1 (DWORD)
  QW_ERROR = Int64(-1); // -1 (QWORD)

  // Error codes returned by BASS_ErrorGetCode()
  BASS_OK = 0; // all is OK
  BASS_ERROR_MEM = 1; // memory error
  BASS_ERROR_FILEOPEN = 2; // can't open the file
  BASS_ERROR_DRIVER = 3; // can't find a free sound driver
  BASS_ERROR_BUFLOST = 4; // the sample buffer was lost
  BASS_ERROR_HANDLE = 5; // invalid handle
  BASS_ERROR_FORMAT = 6; // unsupported sample format
  BASS_ERROR_POSITION = 7; // invalid position
  BASS_ERROR_INIT = 8; // BASS_Init has not been successfully called
  BASS_ERROR_START = 9; // BASS_Start has not been successfully called
  BASS_ERROR_ALREADY = 14; // already initialized/paused/whatever
  BASS_ERROR_NOCHAN = 18; // can't get a free channel
  BASS_ERROR_ILLTYPE = 19; // an illegal type was specified
  BASS_ERROR_ILLPARAM = 20; // an illegal parameter was specified
  BASS_ERROR_NO3D = 21; // no 3D support
  BASS_ERROR_NOEAX = 22; // no EAX support
  BASS_ERROR_DEVICE = 23; // illegal device number
  BASS_ERROR_NOPLAY = 24; // not playing
  BASS_ERROR_FREQ = 25; // illegal sample rate
  BASS_ERROR_NOTFILE = 27; // the stream is not a file stream
  BASS_ERROR_NOHW = 29; // no hardware voices available
  BASS_ERROR_EMPTY = 31; // the MOD music has no sequence data
  BASS_ERROR_NONET = 32; // no internet connection could be opened
  BASS_ERROR_CREATE = 33; // couldn't create the file
  BASS_ERROR_NOFX = 34; // effects are not enabled
  BASS_ERROR_NOTAVAIL = 37; // requested data is not available
  BASS_ERROR_DECODE = 38; // the channel is a "decoding channel"
  BASS_ERROR_DX = 39; // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT = 40; // connection timedout
  BASS_ERROR_FILEFORM = 41; // unsupported file format
  BASS_ERROR_SPEAKER = 42; // unavailable speaker
  BASS_ERROR_VERSION = 43; // invalid BASS version (used by add-ons)
  BASS_ERROR_CODEC = 44; // codec is not available/supported
  BASS_ERROR_ENDED = 45; // the channel/file has ended
  BASS_ERROR_BUSY = 46; // the device is busy
  BASS_ERROR_UNKNOWN = -1; // some other mystery problem

  // BASS_SetConfig options
  BASS_CONFIG_BUFFER = 0;
  BASS_CONFIG_UPDATEPERIOD = 1;
  BASS_CONFIG_GVOL_SAMPLE = 4;
  BASS_CONFIG_GVOL_STREAM = 5;
  BASS_CONFIG_GVOL_MUSIC = 6;
  BASS_CONFIG_CURVE_VOL = 7;
  BASS_CONFIG_CURVE_PAN = 8;
  BASS_CONFIG_FLOATDSP = 9;
  BASS_CONFIG_3DALGORITHM = 10;
  BASS_CONFIG_NET_TIMEOUT = 11;
  BASS_CONFIG_NET_BUFFER = 12;
  BASS_CONFIG_PAUSE_NOPLAY = 13;
  BASS_CONFIG_NET_PREBUF = 15;
  BASS_CONFIG_NET_PASSIVE = 18;
  BASS_CONFIG_REC_BUFFER = 19;
  BASS_CONFIG_NET_PLAYLIST = 21;
  BASS_CONFIG_MUSIC_VIRTUAL = 22;
  BASS_CONFIG_VERIFY = 23;
  BASS_CONFIG_UPDATETHREADS = 24;
  BASS_CONFIG_DEV_BUFFER = 27;
  BASS_CONFIG_DEV_DEFAULT = 36;
  BASS_CONFIG_NET_READTIMEOUT = 37;

  // BASS_SetConfigPtr options
  BASS_CONFIG_NET_AGENT = 16;
  BASS_CONFIG_NET_PROXY = 17;

  // BASS_Init flags
  BASS_DEVICE_8BITS = 1; // use 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO = 2; // use mono, else stereo
  BASS_DEVICE_3D = 4; // enable 3D functionality
  BASS_DEVICE_LATENCY = 256; // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_CPSPEAKERS = 1024; // detect speakers via Windows control panel
  BASS_DEVICE_SPEAKERS = 2048; // force enabling of speaker assignment
  BASS_DEVICE_NOSPEAKER = 4096; // ignore speaker arrangement
  BASS_DEVICE_DMIX = 8192; // use ALSA "dmix" plugin

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS = 1; // IDirectSound
  BASS_OBJECT_DS3DL = 2; // IDirectSound3DListener

  // BASS_DEVICEINFO flags
  BASS_DEVICE_ENABLED = 1;
  BASS_DEVICE_DEFAULT = 2;
  BASS_DEVICE_INIT = 4;

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE = $00000010; // supports all sample rates between min/maxrate
  DSCAPS_EMULDRIVER = $00000020; // device does NOT have hardware DirectSound support
  DSCAPS_CERTIFIED = $00000040; // device driver has been certified by Microsoft
  DSCAPS_SECONDARYMONO = $00000100; // mono
  DSCAPS_SECONDARYSTEREO = $00000200; // stereo
  DSCAPS_SECONDARY8BIT = $00000400; // 8 bit
  DSCAPS_SECONDARY16BIT = $00000800; // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER; // device does NOT have hardware DirectSound recording support
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED; // device driver has been certified by Microsoft

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
  WAVE_FORMAT_1M08 = $00000001; // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08 = $00000002; // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16 = $00000004; // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16 = $00000008; // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08 = $00000010; // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08 = $00000020; // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16 = $00000040; // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16 = $00000080; // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08 = $00000100; // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08 = $00000200; // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16 = $00000400; // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16 = $00000800; // 44.1   kHz, Stereo, 16-bit

  BASS_SAMPLE_8BITS = 1; // 8 bit
  BASS_SAMPLE_FLOAT = 256; // 32-bit floating-point
  BASS_SAMPLE_MONO = 2; // mono
  BASS_SAMPLE_LOOP = 4; // looped
  BASS_SAMPLE_3D = 8; // 3D functionality
  BASS_SAMPLE_SOFTWARE = 16; // not using hardware mixing
  BASS_SAMPLE_MUTEMAX = 32; // mute at max distance (3D only)
  BASS_SAMPLE_VAM = 64; // DX7 voice allocation & management
  BASS_SAMPLE_FX = 128; // old implementation of DX8 effects
  BASS_SAMPLE_OVER_VOL = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST = $30000; // override furthest from listener (3D only)

  BASS_STREAM_PRESCAN = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)
  BASS_MP3_SETPOS = BASS_STREAM_PRESCAN;
  BASS_STREAM_AUTOFREE = $40000; // automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE = $80000; // restrict the download rate of internet file streams
  BASS_STREAM_BLOCK = $100000; // download/play internet file stream in small blocks
  BASS_STREAM_DECODE = $200000; // don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_STATUS = $800000; // give server status info (HTTP/ICY tags) in DOWNLOADPROC

  BASS_MUSIC_FLOAT = BASS_SAMPLE_FLOAT;
  BASS_MUSIC_MONO = BASS_SAMPLE_MONO;
  BASS_MUSIC_LOOP = BASS_SAMPLE_LOOP;
  BASS_MUSIC_3D = BASS_SAMPLE_3D;
  BASS_MUSIC_FX = BASS_SAMPLE_FX;
  BASS_MUSIC_AUTOFREE = BASS_STREAM_AUTOFREE;
  BASS_MUSIC_DECODE = BASS_STREAM_DECODE;
  BASS_MUSIC_PRESCAN = BASS_STREAM_PRESCAN; // calculate playback length
  BASS_MUSIC_CALCLEN = BASS_MUSIC_PRESCAN;
  BASS_MUSIC_RAMP = $200; // normal ramping
  BASS_MUSIC_RAMPS = $400; // sensitive ramping
  BASS_MUSIC_SURROUND = $800; // surround sound
  BASS_MUSIC_SURROUND2 = $1000; // surround sound (mode 2)
  BASS_MUSIC_FT2MOD = $2000; // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD = $4000; // play .MOD as ProTracker 1 does
  BASS_MUSIC_NONINTER = $10000; // non-interpolated sample mixing
  BASS_MUSIC_SINCINTER = $800000; // sinc interpolated sample mixing
  BASS_MUSIC_POSRESET = $8000; // stop all notes when moving position
  BASS_MUSIC_POSRESETEX = $400000; // stop all notes and reset bmp/etc when moving position
  BASS_MUSIC_STOPBACK = $80000; // stop the music on a backwards jump effect
  BASS_MUSIC_NOSAMPLE = $100000; // don't load the samples

  // Speaker assignment flags
  BASS_SPEAKER_FRONT = $1000000; // front speakers
  BASS_SPEAKER_REAR = $2000000; // rear/side speakers
  BASS_SPEAKER_CENLFE = $3000000; // center & LFE speakers (5.1)
  BASS_SPEAKER_REAR2 = $4000000; // rear center speakers (7.1)
  BASS_SPEAKER_LEFT = $10000000; // modifier: left
  BASS_SPEAKER_RIGHT = $20000000; // modifier: right
  BASS_SPEAKER_FRONTLEFT = BASS_SPEAKER_FRONT or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_FRONTRIGHT = BASS_SPEAKER_FRONT or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REARLEFT = BASS_SPEAKER_REAR or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REARRIGHT = BASS_SPEAKER_REAR or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_CENTER = BASS_SPEAKER_CENLFE or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_LFE = BASS_SPEAKER_CENLFE or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REAR2LEFT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REAR2RIGHT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_RIGHT;

  BASS_UNICODE = $80000000;

  BASS_RECORD_PAUSE = $8000; // start recording paused

  // DX7 voice allocation & management flags
  BASS_VAM_HARDWARE = 1;
  BASS_VAM_SOFTWARE = 2;
  BASS_VAM_TERM_TIME = 4;
  BASS_VAM_TERM_DIST = 8;
  BASS_VAM_TERM_PRIO = 16;

  // BASS_CHANNELINFO types
  BASS_CTYPE_SAMPLE = 1;
  BASS_CTYPE_RECORD = 2;
  BASS_CTYPE_STREAM = $10000;
  BASS_CTYPE_STREAM_OGG = $10002;
  BASS_CTYPE_STREAM_MP1 = $10003;
  BASS_CTYPE_STREAM_MP2 = $10004;
  BASS_CTYPE_STREAM_MP3 = $10005;
  BASS_CTYPE_STREAM_AIFF = $10006;
  BASS_CTYPE_STREAM_WAV = $40000; // WAVE flag, LOWORD=codec
  BASS_CTYPE_STREAM_WAV_PCM = $50001;
  BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
  BASS_CTYPE_MUSIC_MOD = $20000;
  BASS_CTYPE_MUSIC_MTM = $20001;
  BASS_CTYPE_MUSIC_S3M = $20002;
  BASS_CTYPE_MUSIC_XM = $20003;
  BASS_CTYPE_MUSIC_IT = $20004;
  BASS_CTYPE_MUSIC_MO3 = $00100; // MO3 flag

  BASS_CTYPE_STREAM_AAC = $10B00; // AAC
  BASS_CTYPE_STREAM_MP4 = $10B01; // MP4
  BASS_CTYPE_STREAM_ALAC = $10E00;
  BASS_CTYPE_STREAM_APE = $10700;
  BASS_CTYPE_STREAM_MPC = $10A00;
  BASS_CTYPE_STREAM_FLAC = $10900;
  BASS_CTYPE_STREAM_FLAC_OGG = $10901;
  BASS_CTYPE_STREAM_MIDI = $10D00;
  BASS_CTYPE_STREAM_OPUS = $11200;
  BASS_CTYPE_STREAM_WV = $10500;

  // 3D channel modes
  BASS_3DMODE_NORMAL = 0; // normal 3D processing
  BASS_3DMODE_RELATIVE = 1; // position is relative to the listener
  BASS_3DMODE_OFF = 2; // no 3D processing

  // software 3D mixing algorithms (used with BASS_CONFIG_3DALGORITHM)
  BASS_3DALG_DEFAULT = 0;
  BASS_3DALG_OFF = 1;
  BASS_3DALG_FULL = 2;
  BASS_3DALG_LIGHT = 3;

  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_GENERIC = 0;
  EAX_ENVIRONMENT_PADDEDCELL = 1;
  EAX_ENVIRONMENT_ROOM = 2;
  EAX_ENVIRONMENT_BATHROOM = 3;
  EAX_ENVIRONMENT_LIVINGROOM = 4;
  EAX_ENVIRONMENT_STONEROOM = 5;
  EAX_ENVIRONMENT_AUDITORIUM = 6;
  EAX_ENVIRONMENT_CONCERTHALL = 7;
  EAX_ENVIRONMENT_CAVE = 8;
  EAX_ENVIRONMENT_ARENA = 9;
  EAX_ENVIRONMENT_HANGAR = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY = 11;
  EAX_ENVIRONMENT_HALLWAY = 12;
  EAX_ENVIRONMENT_STONECORRIDOR = 13;
  EAX_ENVIRONMENT_ALLEY = 14;
  EAX_ENVIRONMENT_FOREST = 15;
  EAX_ENVIRONMENT_CITY = 16;
  EAX_ENVIRONMENT_MOUNTAINS = 17;
  EAX_ENVIRONMENT_QUARRY = 18;
  EAX_ENVIRONMENT_PLAIN = 19;
  EAX_ENVIRONMENT_PARKINGLOT = 20;
  EAX_ENVIRONMENT_SEWERPIPE = 21;
  EAX_ENVIRONMENT_UNDERWATER = 22;
  EAX_ENVIRONMENT_DRUGGED = 23;
  EAX_ENVIRONMENT_DIZZY = 24;
  EAX_ENVIRONMENT_PSYCHOTIC = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT = 26;

  BASS_STREAMPROC_END = $80000000; // end of user stream flag

  // BASS_StreamCreateFileUser file systems
  STREAMFILE_NOBUFFER = 0;
  STREAMFILE_BUFFER = 1;
  STREAMFILE_BUFFERPUSH = 2;

  // BASS_StreamPutFileData options
  BASS_FILEDATA_END = 0; // end & close the file

  // BASS_StreamGetFilePosition modes
  BASS_FILEPOS_CURRENT = 0;
  BASS_FILEPOS_DECODE = BASS_FILEPOS_CURRENT;
  BASS_FILEPOS_DOWNLOAD = 1;
  BASS_FILEPOS_END = 2;
  BASS_FILEPOS_START = 3;
  BASS_FILEPOS_CONNECTED = 4;
  BASS_FILEPOS_BUFFER = 5;

  // BASS_ChannelSetSync types
  BASS_SYNC_POS = 0;
  BASS_SYNC_END = 2;
  BASS_SYNC_META = 4;
  BASS_SYNC_SLIDE = 5;
  BASS_SYNC_STALL = 6;
  BASS_SYNC_DOWNLOAD = 7;
  BASS_SYNC_FREE = 8;
  BASS_SYNC_SETPOS = 11;
  BASS_SYNC_MUSICPOS = 10;
  BASS_SYNC_MUSICINST = 1;
  BASS_SYNC_MUSICFX = 3;
  BASS_SYNC_OGG_CHANGE = 12;
  BASS_SYNC_MIXTIME = $40000000; // FLAG: sync at mixtime, else at playtime
  BASS_SYNC_ONETIME = $80000000; // FLAG: sync only once, else continuously

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED = 3;

  // Channel attributes
  BASS_ATTRIB_FREQ = 1;
  BASS_ATTRIB_VOL = 2;
  BASS_ATTRIB_PAN = 3;
  BASS_ATTRIB_EAXMIX = 4;
  BASS_ATTRIB_NOBUFFER = 5;
  BASS_ATTRIB_CPU = 7;
  BASS_ATTRIB_MUSIC_AMPLIFY = $100;
  BASS_ATTRIB_MUSIC_PANSEP = $101;
  BASS_ATTRIB_MUSIC_PSCALER = $102;
  BASS_ATTRIB_MUSIC_BPM = $103;
  BASS_ATTRIB_MUSIC_SPEED = $104;
  BASS_ATTRIB_MUSIC_VOL_GLOBAL = $105;
  BASS_ATTRIB_MUSIC_VOL_CHAN = $200; // + channel #
  BASS_ATTRIB_MUSIC_VOL_INST = $300; // + instrument #

  // BASS_ChannelGetData flags
  BASS_DATA_AVAILABLE = 0; // query how much data is buffered
  BASS_DATA_FLOAT = $40000000; // flag: return floating-point sample data
  BASS_DATA_FFT256 = $80000000; // 256 sample FFT
  BASS_DATA_FFT512 = $80000001; // 512 FFT
  BASS_DATA_FFT1024 = $80000002; // 1024 FFT
  BASS_DATA_FFT2048 = $80000003; // 2048 FFT
  BASS_DATA_FFT4096 = $80000004; // 4096 FFT
  BASS_DATA_FFT8192 = $80000005; // 8192 FFT
  BASS_DATA_FFT16384 = $80000006; // 16384 FFT
  BASS_DATA_FFT_INDIVIDUAL = $10; // FFT flag: FFT for each channel, else all combined
  BASS_DATA_FFT_NOWINDOW = $20; // FFT flag: no Hanning window
  BASS_DATA_FFT_REMOVEDC = $40; // FFT flag: pre-remove DC bias

  // BASS_ChannelGetTags types : what's returned
  BASS_TAG_ID3 = 0; // ID3v1 tags : TAG_ID3 structure
  BASS_TAG_ID3V2 = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG = 2; // OGG comments : series of null-terminated UTF-8 strings
  BASS_TAG_HTTP = 3; // HTTP headers : series of null-terminated ANSI strings
  BASS_TAG_ICY = 4; // ICY headers : series of null-terminated ANSI strings
  BASS_TAG_META = 5; // ICY metadata : ANSI string
  BASS_TAG_APE = 6; // APEv2 tags : series of null-terminated UTF-8 strings
  BASS_TAG_MP4 = 7; // MP4/iTunes metadata : series of null-terminated UTF-8 strings
  BASS_TAG_VENDOR = 9; // OGG encoder : UTF-8 string
  BASS_TAG_LYRICS3 = 10; // Lyric3v2 tag : ASCII string
  BASS_TAG_CA_CODEC = 11; // CoreAudio codec info : TAG_CA_CODEC structure
  BASS_TAG_MF = 13; // Media Foundation tags : series of null-terminated UTF-8 strings
  BASS_TAG_WAVEFORMAT = 14; // WAVE format : WAVEFORMATEEX structure
  BASS_TAG_RIFF_INFO = $100; // RIFF "INFO" tags : series of null-terminated ANSI strings
  BASS_TAG_RIFF_BEXT = $101; // RIFF/BWF "bext" tags : TAG_BEXT structure
  BASS_TAG_RIFF_CART = $102; // RIFF/BWF "cart" tags : TAG_CART structure
  BASS_TAG_RIFF_DISP = $103; // RIFF "DISP" text tag : ANSI string
  BASS_TAG_APE_BINARY = $1000; // + index #, binary APEv2 tag : TAG_APE_BINARY structure
  BASS_TAG_MUSIC_NAME = $10000; // MOD music name : ANSI string
  BASS_TAG_MUSIC_MESSAGE = $10001; // MOD message : ANSI string
  BASS_TAG_MUSIC_ORDERS = $10002; // MOD order list : BYTE array of pattern numbers
  BASS_TAG_MUSIC_INST = $10100; // + instrument #, MOD instrument name : ANSI string
  BASS_TAG_MUSIC_SAMPLE = $10300; // + sample #, MOD sample name : ANSI string

  // BASS_ChannelGetLength/GetPosition/SetPosition modes
  BASS_POS_BYTE = 0; // byte position
  BASS_POS_MUSIC_ORDER = 1; // order.row position, MAKELONG(order,row)
  BASS_POS_DECODE = $10000000; // flag: get the decoding (not playing) position
  BASS_POS_DECODETO = $20000000; // flag: decode to the position instead of seeking

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF = $10000;
  BASS_INPUT_ON = $20000;

  BASS_INPUT_TYPE_MASK = $FF000000;
  BASS_INPUT_TYPE_UNDEF = $00000000;
  BASS_INPUT_TYPE_DIGITAL = $01000000;
  BASS_INPUT_TYPE_LINE = $02000000;
  BASS_INPUT_TYPE_MIC = $03000000;
  BASS_INPUT_TYPE_SYNTH = $04000000;
  BASS_INPUT_TYPE_CD = $05000000;
  BASS_INPUT_TYPE_PHONE = $06000000;
  BASS_INPUT_TYPE_SPEAKER = $07000000;
  BASS_INPUT_TYPE_WAVE = $08000000;
  BASS_INPUT_TYPE_AUX = $09000000;
  BASS_INPUT_TYPE_ANALOG = $0A000000;

  BASS_FX_DX8_CHORUS = 0;
  BASS_FX_DX8_COMPRESSOR = 1;
  BASS_FX_DX8_DISTORTION = 2;
  BASS_FX_DX8_ECHO = 3;
  BASS_FX_DX8_FLANGER = 4;
  BASS_FX_DX8_GARGLE = 5;
  BASS_FX_DX8_I3DL2REVERB = 6;
  BASS_FX_DX8_PARAMEQ = 7;
  BASS_FX_DX8_REVERB = 8;

  BASS_DX8_PHASE_NEG_180 = 0;
  BASS_DX8_PHASE_NEG_90 = 1;
  BASS_DX8_PHASE_ZERO = 2;
  BASS_DX8_PHASE_90 = 3;
  BASS_DX8_PHASE_180 = 4;

type
  DWORD = LongWord;
  BOOL = LongBool;
  FLOAT = Single;
  QWORD = Int64;

  HMUSIC = DWORD; // MOD music handle
  HSAMPLE = DWORD; // sample handle
  HCHANNEL = DWORD; // playing sample's channel handle
  HSTREAM = DWORD; // sample stream handle
  HRECORD = DWORD; // recording handle
  HSYNC = DWORD; // synchronizer handle
  HDSP = DWORD; // DSP handle
  HFX = DWORD; // DX8 effect handle
  HPLUGIN = DWORD; // Plugin handle

  // Channel info structure
  BASS_CHANNELINFO = record
    freq: DWORD; // default playback rate
    chans: DWORD; // channels
    flags: DWORD; // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: DWORD; // type of channel
    origres: DWORD; // original resolution
    plugin: DWORD; // plugin
    sample: DWORD; // sample
    filename: PChar; // filename
  end;

  // Device info structure
  BASS_DEVICEINFO = record
{$IFDEF NEXTGEN}
    name: PByte; // description
    driver: PByte; // driver
{$ELSE}
    name: PAnsiChar; // description
    driver: PAnsiChar; // driver
{$ENDIF}
    flags: DWORD;
  end;

const
  // special STREAMPROCs
  STREAMPROC_DUMMY = Pointer(0); // "dummy" stream
  STREAMPROC_PUSH = Pointer(-1); // push stream

type

  DOWNLOADPROC = procedure(buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
  }

  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: DWORD; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }

const
  // additional BASS_SetConfig option
  BASS_CONFIG_MIXER_FILTER = $10600;
  BASS_CONFIG_MIXER_BUFFER = $10601;
  BASS_CONFIG_MIXER_POSEX = $10602;
  BASS_CONFIG_SPLIT_BUFFER = $10610;

  // BASS_Mixer_StreamCreate flags
  BASS_MIXER_END = $10000; // end the stream when there are no sources
  BASS_MIXER_NONSTOP = $20000; // don't stall when there are no sources
  BASS_MIXER_RESUME = $1000; // resume stalled immediately upon new/unpaused source
  BASS_MIXER_POSEX = $2000; // enable BASS_Mixer_ChannelGetPositionEx support

  // source flags
  BASS_MIXER_FILTER = $1000; // resampling filter
  BASS_MIXER_BUFFER = $2000; // buffer data for BASS_Mixer_ChannelGetData/Level
  BASS_MIXER_LIMIT = $4000; // limit mixer processing to the amount available from this source
  BASS_MIXER_MATRIX = $10000; // matrix mixing
  BASS_MIXER_PAUSE = $20000; // don't process the source
  BASS_MIXER_DOWNMIX = $400000; // downmix to stereo/mono
  BASS_MIXER_NORAMPIN = $800000; // don't ramp-in the start

  // splitter flags
  BASS_SPLIT_SLAVE = $1000; // only read buffered data

  // envelope types
  BASS_MIXER_ENV_FREQ = 1;
  BASS_MIXER_ENV_VOL = 2;
  BASS_MIXER_ENV_PAN = 3;
  BASS_MIXER_ENV_LOOP = $10000; // FLAG: loop

  // additional sync type
  BASS_SYNC_MIXER_ENVELOPE = $10200;
  BASS_SYNC_MIXER_ENVELOPE_NODE = $10201;

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_MIXER = $10800;
  BASS_CTYPE_STREAM_SPLIT = $10801;

type
  // envelope node
  BASS_MIXER_NODE = record
    pos: QWORD;
    value: Single;
  end;

  PBASS_MIXER_NODE = ^BASS_MIXER_NODE;

  // * BASS_FX

const
  // DSP channels flags
  BASS_BFX_CHANALL = -1; // all channels at once (as by default)
  BASS_BFX_CHANNONE = 0; // disable an effect for all channels
  BASS_BFX_CHAN1 = 1; // left-front channel
  BASS_BFX_CHAN2 = 2; // right-front channel
  BASS_BFX_CHAN3 = 4; // see above info
  BASS_BFX_CHAN4 = 8; // see above info
  BASS_BFX_CHAN5 = 16; // see above info
  BASS_BFX_CHAN6 = 32; // see above info
  BASS_BFX_CHAN7 = 64; // see above info
  BASS_BFX_CHAN8 = 128; // see above info

  // If you have more than 8 channels, use this macro
function BASS_BFX_CHANNEL_N(n: DWORD): DWORD;

// DSP effects
const
  BASS_FX_BFX_ROTATE = $10000; // A channels volume ping-pong  / stereo
  BASS_FX_BFX_ECHO = $10001; // Echo                         / 2 channels max
  BASS_FX_BFX_FLANGER = $10002; // Flanger                      / multi channel
  BASS_FX_BFX_VOLUME = $10003; // Volume                       / multi channel
  BASS_FX_BFX_PEAKEQ = $10004; // Peaking Equalizer            / multi channel
  BASS_FX_BFX_REVERB = $10005; // Reverb                       / 2 channels max
  BASS_FX_BFX_LPF = $10006; // Low Pass Filter 24dB         / multi channel
  BASS_FX_BFX_MIX = $10007; // Swap, remap and mix channels / multi channel
  BASS_FX_BFX_DAMP = $10008; // Dynamic Amplification        / multi channel
  BASS_FX_BFX_AUTOWAH = $10009; // Auto WAH                     / multi channel
  BASS_FX_BFX_ECHO2 = $1000A; // Echo 2                       / multi channel
  BASS_FX_BFX_PHASER = $1000B; // Phaser                       / multi channel
  BASS_FX_BFX_ECHO3 = $1000C; // Echo 3                       / multi channel
  BASS_FX_BFX_CHORUS = $1000D; // Chorus                       / multi channel
  BASS_FX_BFX_APF = $1000E; // All Pass Filter              / multi channel
  BASS_FX_BFX_COMPRESSOR = $1000F; // Compressor                   / multi channel
  BASS_FX_BFX_DISTORTION = $10010; // Distortion                   / multi channel
  BASS_FX_BFX_COMPRESSOR2 = $10011; // Compressor 2                 / multi channel
  BASS_FX_BFX_VOLUME_ENV = $10012; // Volume envelope              / multi channel
  BASS_FX_BFX_BQF = $10013; // BiQuad filters               / multi channel

  // BiQuad filters
const
  BASS_BFX_BQF_LOWPASS = 0;
  BASS_BFX_BQF_HIGHPASS = 1;
  BASS_BFX_BQF_BANDPASS = 2; // constant 0 dB peak gain
  BASS_BFX_BQF_BANDPASS_Q = 3; // constant skirt gain, peak gain = Q
  BASS_BFX_BQF_NOTCH = 4;
  BASS_BFX_BQF_ALLPASS = 5;
  BASS_BFX_BQF_PEAKINGEQ = 6;
  BASS_BFX_BQF_LOWSHELF = 7;
  BASS_BFX_BQF_HIGHSHELF = 8;

type
  // Echo
  BASS_BFX_ECHO = record
    fLevel: FLOAT; // [0....1....n] linear
    lDelay: Integer; // [1200..30000]
  end;

  // Flanger
  BASS_BFX_FLANGER = record
    fWetDry: FLOAT; // [0....1....n] linear
    fSpeed: FLOAT; // [0......0.09]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Volume
  BASS_BFX_VOLUME = record
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s or 0 for global volume control
    fVolume: FLOAT; // [0....1....n] linear
  end;

  // Peaking Equalizer
  BASS_BFX_PEAKEQ = record
    lBand: Integer; // [0...............n] more bands means more memory & cpu usage
    fBandwidth: FLOAT; // [0.1...........<10] in octaves - fQ is not in use (Bandwidth has a priority over fQ)
    fQ: FLOAT; // [0...............1] the EE kinda definition (linear) (if Bandwidth is not in use)
    fCenter: FLOAT; // [1Hz..<info.freq/2] in Hz
    fGain: FLOAT; // [-15dB...0...+15dB] in dB
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Reverb
  BASS_BFX_REVERB = record
    fLevel: FLOAT; // [0....1....n] linear
    lDelay: Integer; // [1200..10000]
  end;

  // Low Pass Filter
  BASS_BFX_LPF = record
    fResonance: FLOAT; // [0.01............10]
    fCutOffFreq: FLOAT; // [1Hz....info.freq/2] cutoff frequency
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Swap, remap and mix
  PTlChannel = ^TlChannel;
  TlChannel = array [0 .. maxInt div sizeOf(DWORD) - 1] of DWORD;

  BASS_BFX_MIX = record
    lChannel: PTlChannel;
    // a pointer to an array of channels to mix using BASS_BFX_CHANxxx flag/s (lChannel[0] is left channel...)
  end;

  // Dynamic Amplification
  BASS_BFX_DAMP = record
    fTarget: FLOAT; // target volume level                      [0<......1] linear
    fQuiet: FLOAT; // quiet  volume level                      [0.......1] linear
    fRate: FLOAT; // amp adjustment rate                      [0.......1] linear
    fGain: FLOAT; // amplification level                      [0...1...n] linear
    fDelay: FLOAT; // delay in seconds before increasing level [0.......n] linear
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Auto WAH
  BASS_BFX_AUTOWAH = record
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-2......2]
    fWetMix: FLOAT; // wet (affected) signal mix                [-2......2]
    fFeedback: FLOAT; // feedback                                 [-1......1]
    fRate: FLOAT; // rate of sweep in cycles per second       [0<....<10]
    fRange: FLOAT; // sweep range in octaves                   [0<....<10]
    fFreq: FLOAT; // base frequency of sweep Hz               [0<...1000]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Echo 2
  BASS_BFX_ECHO2 = record
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-2......2]
    fWetMix: FLOAT; // wet (affected) signal mix                [-2......2]
    fFeedback: FLOAT; // feedback                                 [-1......1]
    fDelay: FLOAT; // delay sec                                [0<......n]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Phaser
  BASS_BFX_PHASER = record
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-2......2]
    fWetMix: FLOAT; // wet (affected) signal mix                [-2......2]
    fFeedback: FLOAT; // feedback                                 [-1......1]
    fRate: FLOAT; // rate of sweep in cycles per second       [0<....<10]
    fRange: FLOAT; // sweep range in octaves                   [0<....<10]
    fFreq: FLOAT; // base frequency of sweep                  [0<...1000]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Echo 3
  BASS_BFX_ECHO3 = record
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-2......2]
    fWetMix: FLOAT; // wet (affected) signal mix                [-2......2]
    fDelay: FLOAT; // delay sec                                [0<......n]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Chorus
  BASS_BFX_CHORUS = record
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-2......2]
    fWetMix: FLOAT; // wet (affected) signal mix                [-2......2]
    fFeedback: FLOAT; // feedback                                 [-1......1]
    fMinSweep: FLOAT; // minimal delay ms                         [0<..<6000]
    fMaxSweep: FLOAT; // maximum delay ms                         [0<..<6000]
    fRate: FLOAT; // rate ms/s                                [0<...1000]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // All Pass Filter
  BASS_BFX_APF = record
    fGain: FLOAT; // reverberation time                       [-1=<..<=1]
    fDelay: FLOAT; // delay sec                                [0<....<=n]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Compressor
  BASS_BFX_COMPRESSOR = record
    fThreshold: FLOAT; // compressor threshold                     [0<=...<=1]
    fAttacktime: FLOAT; // attack time ms                           [0<.<=1000]
    fReleasetime: FLOAT; // release time ms                          [0<.<=5000]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  // Distortion
  BASS_BFX_DISTORTION = record
    fDrive: FLOAT; // distortion drive                         [0<=...<=5]
    fDryMix: FLOAT; // dry (unaffected) signal mix              [-5<=..<=5]
    fWetMix: FLOAT; // wet (affected) signal mix                [-5<=..<=5]
    fFeedback: FLOAT; // feedback                                 [-1<=..<=1]
    fVolume: FLOAT; // distortion volume                        [0=<...<=2]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  BASS_INFO = record
    flags: DWORD; // device capabilities (DSCAPS_xxx flags)
    hwsize: DWORD; // size of total device hardware memory
    hwfree: DWORD; // size of free device hardware memory
    freesam: DWORD; // number of free sample slots in the hardware
    free3d: DWORD; // number of free 3D sample slots in the hardware
    minrate: DWORD; // min sample rate supported by the hardware
    maxrate: DWORD; // max sample rate supported by the hardware
    eax: BOOL; // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: DWORD; // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: DWORD; // DirectSound version
    latency: DWORD; // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    initflags: DWORD; // BASS_Init "flags" parameter
    speakers: DWORD; // number of speakers available
    freq: DWORD; // current output rate (OSX only)
  end;

  // Compressor 2
  BASS_BFX_COMPRESSOR2 = record
    fGain: FLOAT; // output gain of signal after compression  [-60....60] in dB
    fThreshold: FLOAT; // point at which compression begins        [-60.....0] in dB
    fRatio: FLOAT; // compression ratio                        [1.......n]
    fAttack: FLOAT; // attack time in ms                        [0.01.1000]
    fRelease: FLOAT; // release time in ms                       [0.01.5000]
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

  BASS_PLUGINFORM = record
    ctype: DWORD; // channel type
    name: PChar; // format description
    exts: PChar; // file extension filter (*.ext1;*.ext2;etc...)
  end;

  PBASS_PLUGINFORMS = ^TBASS_PLUGINFORMS;
  TBASS_PLUGINFORMS = array [0 .. maxInt div sizeOf(BASS_PLUGINFORM) - 1] of BASS_PLUGINFORM;

  BASS_PLUGININFO = record
    version: DWORD; // version (same form as BASS_GetVersion)
    formatc: DWORD; // number of formats
    formats: PBASS_PLUGINFORMS; // the array of formats
  end;

  PBASS_PLUGININFO = ^BASS_PLUGININFO;

  // Volume envelope
  BASS_BFX_ENV_NODE = packed record
    pos: DOUBLE; // node position in seconds (1st envelope node must be at position 0)
    val: FLOAT; // node value
  end;

  PBASS_BFX_ENV_NODES = ^TBASS_BFX_ENV_NODES;
  TBASS_BFX_ENV_NODES = array [0 .. maxInt div sizeOf(BASS_BFX_ENV_NODE) - 1] of BASS_BFX_ENV_NODE;

  BASS_BFX_VOLUME_ENV = record
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
    lNodeCount: Integer; // number of nodes
    pNodes: PBASS_BFX_ENV_NODES; // the nodes
    bFollow: BOOL; // follow source position
  end;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = record
    x: FLOAT; // +=right, -=left
    y: FLOAT; // +=up, -=down
    z: FLOAT; // +=front, -=behind
  end;

  // BiQuad Filters
  BASS_BFX_BQF = record
    lFilter: Integer; // BASS_BFX_BQF_xxx filter types
    fCenter: FLOAT; // [1Hz..<info.freq/2] Cutoff (central) frequency in Hz
    fGain: FLOAT; // [-15dB...0...+15dB] Used only for PEAKINGEQ and Shelving filters in dB
    fBandwidth: FLOAT; // [0.1...........<10] Bandwidth in octaves (fQ is not in use (fBandwidth has a priority over fQ))
    // (between -3 dB frequencies for BANDPASS and NOTCH or between midpoint
    // (fGgain/2) gain frequencies for PEAKINGEQ)
    fQ: FLOAT; // [0.1.............1] The EE kinda definition (linear) (if fBandwidth is not in use)
    fS: FLOAT; // [0.1.............1] A "shelf slope" parameter (linear) (used only with Shelving filters)
    // when fS = 1, the shelf slope is as steep as you can get it and remain monotonically
    // increasing or decreasing gain with frequency.
    lChannel: Integer; // BASS_BFX_CHANxxx flag/s
  end;

function BassLoaded: boolean;
// function TagsLoaded: boolean;
function LoadBASS: boolean;
function LoadBASSPlugins: boolean;
procedure UnloadBASS;
function GetBASSFileInfo(filename: String; var Info: BASS_CHANNELINFO; var PlayTime: Int64): boolean;
function GetBASSStreamType(ctype: Cardinal): String;
function PByte2String(Bytes: PByte): String;

var
  BASSLibraryHandle: DWORD = 0;
  // BASSTagsHandle: DWORD = 0;

  // * BASS
  BASS_SampleSetData: function(handle: HSAMPLE; buffer: Pointer): BOOL; stdcall;
  BASS_SampleFree: function(handle: HSAMPLE): BOOL; stdcall;
  BASS_SampleCreate: function(length, freq, chans, max, flags: DWORD): HSAMPLE; stdcall;
  BASS_SampleLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; stdcall;
  BASS_GetEAXParameters: function(var env: DWORD; var vol, decay, damp: FLOAT): BOOL; stdcall;
  BASS_SetEAXParameters: function(env: Integer; vol, decay, damp: FLOAT): BOOL; stdcall;
  BASS_Apply3D: procedure; stdcall;
  BASS_Get3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; stdcall;
  BASS_Set3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; stdcall;
  BASS_Get3DFactors: function(var distf, rollf, doppf: FLOAT): BOOL; stdcall;
  BASS_Set3DFactors: function(distf, rollf, doppf: FLOAT): BOOL; stdcall;
  BASS_PluginGetInfo: function(handle: HPLUGIN): PBASS_PLUGININFO; stdcall;
  BASS_PluginFree: function(handle: HPLUGIN): BOOL; stdcall;
  BASS_GetVolume: function: FLOAT; stdcall;
  BASS_SetVolume: function(volume: FLOAT): BOOL; stdcall;
  BASS_Pause: function: LongBool; stdcall;
  BASS_Start: function: LongBool; stdcall;
  BASS_Update: function(length: DWORD): LongBool; stdcall;
  BASS_GetInfo: function(var Info: BASS_INFO): LongBool; stdcall;
  BASS_GetDSoundObject: function(obj: DWORD): Pointer; stdcall;
  BASS_SetDevice: function(device: DWORD): LongBool; stdcall;
  BASS_GetVersion: function: DWORD; stdcall;
  BASS_GetConfigPtr: function(option: DWORD): Pointer; cdecl;
  BASS_SetConfigPtr: function(option: DWORD; value: Pointer): LongBool; cdecl;
  BASS_Init: function(device: LongInt; freq, flags: DWORD; win: Pointer; clsid: Pointer): LongBool; cdecl;
  BASS_StreamCreateFile: function(mem: LongBool; f: Pointer; offset, length: UInt64; flags: DWORD): Cardinal; cdecl;
  BASS_ChannelPlay: function(handle: DWORD; restart: LongBool): LongBool; cdecl;
  BASS_ErrorGetCode: function: LongInt; cdecl;
  BASS_Free: function: LongBool; cdecl;
  BASS_GetCPU: function: Single; cdecl;
  BASS_SetConfig: function(option, value: DWORD): LongBool; cdecl;
  BASS_GetConfig: function(option: DWORD): DWORD; cdecl;
  BASS_GetDevice: function: DWORD; cdecl;
  BASS_ChannelGetInfo: function(handle: DWORD; var Info: BASS_CHANNELINFO): LongBool; cdecl;
  BASS_MusicLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, flags, freq: DWORD): HMUSIC; cdecl;
  BASS_MusicFree: function(handle: HMUSIC): LongBool; cdecl;
  BASS_StreamFree: function(handle: HSTREAM): LongBool; cdecl;
  BASS_ChannelBytes2Seconds: function(handle: DWORD; pos: QWORD): DOUBLE; cdecl;
  BASS_ChannelGetLength: function(handle, mode: DWORD): QWORD; cdecl;
  BASS_ChannelGetPosition: function(handle, mode: DWORD): QWORD; cdecl;
  BASS_ChannelStop: function(handle: DWORD): BOOL; cdecl;
  BASS_ChannelPause: function(handle: DWORD): BOOL; cdecl;
  BASS_PluginLoad: function(filename: PChar; flags: DWORD): HPLUGIN; cdecl;
  BASS_ChannelSetSync: function(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; cdecl;
  BASS_GetDeviceInfo: function(device: DWORD; var Info: BASS_DEVICEINFO): BOOL; cdecl;
  BASS_ChannelSetPosition: function(handle: DWORD; pos: QWORD; mode: DWORD): BOOL; cdecl;
  BASS_ChannelRemoveSync: function(handle: DWORD; sync: HSYNC): BOOL; cdecl;
  BASS_ChannelSeconds2Bytes: function(handle: DWORD; pos: DOUBLE): QWORD; cdecl;
  BASS_ChannelLock: function(handle: DWORD; lock: BOOL): BOOL; cdecl;
  BASS_StreamCreateURL: function(url: Pointer; offset: DWORD; flags: DWORD; proc: DOWNLOADPROC; user: Pointer): HSTREAM; cdecl;
  BASS_Stop: function: BOOL; cdecl;
  BASS_ChannelGetTags: function(handle: HSTREAM; tags: DWORD): PByte; cdecl;
  BASS_ChannelSetFX: function(handle, type_: DWORD; priority: LongInt): HFX; cdecl;
  BASS_ChannelRemoveFX: function(handle: DWORD; fx: HFX): BOOL; cdecl;
  BASS_FXSetParameters: function(handle: HFX; par: Pointer): BOOL; cdecl;
  BASS_FXGetParameters: function(handle: HFX; par: Pointer): BOOL; cdecl;
  BASS_ChannelIsActive: function(handle: DWORD): DWORD; cdecl;
  BASS_StreamGetFilePosition: function(handle: HSTREAM; mode: DWORD): QWORD; cdecl;

  TAGS_GetVersion: function: DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TAGS_SetUTF8: function(enable: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TAGS_Read: function(handle: DWORD; const fmt: Pointer): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TAGS_ReadEx: function(handle: DWORD; const fmt: Pointer; tagtype: DWORD; codepage: LongInt): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TAGS_GetLastErrorDesc: function: Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

implementation

uses System.IOUtils;

function BassLoaded: boolean;
begin
  Result := BASSLibraryHandle > 0;
end;

// function TagsLoaded: boolean;
// begin
// Result := BASSTagsHandle > 0;
// end;

function LoadBASS: boolean;
var
  DocDir: string;
begin
  Result := False;
{$IFDEF ANDROID}
  DocDir := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetDocumentsPath);
{$ELSE}
  DocDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$ENDIF}
  // * BASS
  // BASSTagsHandle := LoadLibrary(PChar(DocDir + {$IFDEF ANDROID} 'libtags.so' {$ELSE} 'tags.dll'{$ENDIF}));
  BASSLibraryHandle := LoadLibrary(PChar(DocDir + {$IFDEF ANDROID} 'libbass.so' {$ELSE} 'bass.dll'{$ENDIF}));
  if BASSLibraryHandle = 0 then
    Exit;

  if BASSLibraryHandle <> 0 then
  begin
    @BASS_SampleSetData := GetProcAddress(BASSLibraryHandle, ('BASS_SampleSetData'));
    @BASS_SampleFree := GetProcAddress(BASSLibraryHandle, ('BASS_SampleFree'));
    @BASS_SampleCreate := GetProcAddress(BASSLibraryHandle, ('BASS_SampleCreate'));
    @BASS_SampleLoad := GetProcAddress(BASSLibraryHandle, ('BASS_SampleLoad'));
    @BASS_GetEAXParameters := GetProcAddress(BASSLibraryHandle, ('BASS_GetEAXParameters'));
    @BASS_SetEAXParameters := GetProcAddress(BASSLibraryHandle, ('BASS_SetEAXParameters'));
    @BASS_Apply3D := GetProcAddress(BASSLibraryHandle, ('BASS_Apply3D'));
    @BASS_Get3DPosition := GetProcAddress(BASSLibraryHandle, ('BASS_Get3DPosition'));
    @BASS_Set3DPosition := GetProcAddress(BASSLibraryHandle, ('BASS_Set3DPosition'));
    @BASS_Get3DFactors := GetProcAddress(BASSLibraryHandle, ('BASS_Get3DFactors'));
    @BASS_Set3DFactors := GetProcAddress(BASSLibraryHandle, ('BASS_Set3DFactors'));
    @BASS_PluginGetInfo := GetProcAddress(BASSLibraryHandle, ('BASS_PluginGetInfo'));
    @BASS_PluginFree := GetProcAddress(BASSLibraryHandle, ('BASS_PluginFree'));
    @BASS_GetVolume := GetProcAddress(BASSLibraryHandle, ('BASS_GetVolume'));
    @BASS_SetVolume := GetProcAddress(BASSLibraryHandle, ('BASS_SetVolume'));
    @BASS_Pause := GetProcAddress(BASSLibraryHandle, ('BASS_Pause'));
    @BASS_Start := GetProcAddress(BASSLibraryHandle, ('BASS_Start'));
    @BASS_Update := GetProcAddress(BASSLibraryHandle, ('BASS_Update'));
    @BASS_GetInfo := GetProcAddress(BASSLibraryHandle, ('BASS_GetInfo'));
    @BASS_GetDSoundObject := GetProcAddress(BASSLibraryHandle, ('BASS_GetDSoundObject'));
    @BASS_SetDevice := GetProcAddress(BASSLibraryHandle, ('BASS_SetDevice'));
    @BASS_GetVersion := GetProcAddress(BASSLibraryHandle, ('BASS_GetVersion'));
    @BASS_GetConfigPtr := GetProcAddress(BASSLibraryHandle, ('BASS_GetConfigPtr'));
    @BASS_SetConfigPtr := GetProcAddress(BASSLibraryHandle, ('BASS_SetConfigPtr'));
    @BASS_Init := GetProcAddress(BASSLibraryHandle, ('BASS_Init'));
    @BASS_StreamCreateFile := GetProcAddress(BASSLibraryHandle, ('BASS_StreamCreateFile'));
    @BASS_ChannelPlay := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelPlay'));
    @BASS_ErrorGetCode := GetProcAddress(BASSLibraryHandle, ('BASS_ErrorGetCode'));
    @BASS_Free := GetProcAddress(BASSLibraryHandle, ('BASS_Free'));
    @BASS_GetCPU := GetProcAddress(BASSLibraryHandle, ('BASS_GetCPU'));
    @BASS_SetConfig := GetProcAddress(BASSLibraryHandle, ('BASS_SetConfig'));
    @BASS_GetConfig := GetProcAddress(BASSLibraryHandle, ('BASS_GetConfig'));
    @BASS_GetDevice := GetProcAddress(BASSLibraryHandle, ('BASS_GetDevice'));
    @BASS_ChannelGetInfo := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelGetInfo'));
    @BASS_MusicLoad := GetProcAddress(BASSLibraryHandle, ('BASS_MusicLoad'));
    @BASS_MusicFree := GetProcAddress(BASSLibraryHandle, ('BASS_MusicFree'));
    @BASS_StreamFree := GetProcAddress(BASSLibraryHandle, ('BASS_StreamFree'));
    @BASS_ChannelBytes2Seconds := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelBytes2Seconds'));
    @BASS_ChannelGetLength := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelGetLength'));
    @BASS_ChannelGetPosition := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelGetPosition'));
    @BASS_ChannelStop := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelStop'));
    @BASS_ChannelPause := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelPause'));
    @BASS_PluginLoad := GetProcAddress(BASSLibraryHandle, ('BASS_PluginLoad'));
    @BASS_ChannelSetSync := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelSetSync'));
    @BASS_GetDeviceInfo := GetProcAddress(BASSLibraryHandle, ('BASS_GetDeviceInfo'));
    @BASS_ChannelSetPosition := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelSetPosition'));
    @BASS_ChannelRemoveSync := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelRemoveSync'));
    @BASS_ChannelSeconds2Bytes := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelSeconds2Bytes'));
    @BASS_ChannelLock := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelLock'));
    @BASS_StreamCreateURL := GetProcAddress(BASSLibraryHandle, ('BASS_StreamCreateURL'));
    @BASS_Stop := GetProcAddress(BASSLibraryHandle, ('BASS_Stop'));
    @BASS_ChannelGetTags := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelGetTags'));
    @BASS_ChannelSetFX := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelSetFX'));
    @BASS_ChannelRemoveFX := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelRemoveFX'));
    @BASS_FXSetParameters := GetProcAddress(BASSLibraryHandle, ('BASS_FXSetParameters'));
    @BASS_FXGetParameters := GetProcAddress(BASSLibraryHandle, ('BASS_FXGetParameters'));
    @BASS_ChannelIsActive := GetProcAddress(BASSLibraryHandle, ('BASS_ChannelIsActive'));
    @BASS_StreamGetFilePosition := GetProcAddress(BASSLibraryHandle, ('BASS_StreamGetFilePosition'));
    Result := True;
  end;

  { if BASSTagsHandle <> 0 then
    begin
    @TAGS_GetVersion := GetProcAddress(BASSTagsHandle, ('TAGS_GetVersion'));
    @TAGS_SetUTF8 := GetProcAddress(BASSTagsHandle, ('TAGS_SetUTF8'));
    @TAGS_Read := GetProcAddress(BASSTagsHandle, ('TAGS_Read'));
    @TAGS_ReadEx := GetProcAddress(BASSTagsHandle, ('TAGS_ReadEx'));
    @TAGS_GetLastErrorDesc := GetProcAddress(BASSTagsHandle, ('TAGS_GetLastErrorDesc'));
    end; }

end;

procedure UnloadBASS;
begin
  if BASSLibraryHandle <> 0 then
  begin
    BASS_Free; // make sure we release everything
    FreeLibrary(BASSLibraryHandle);
  end;
  BASSLibraryHandle := 0;

  // FreeLibrary(BASSTagsHandle);
  // BASSTagsHandle := 0;
end;

function LoadBASSPlugins: boolean;
var
  DocDir: string;
begin
{$IFDEF ANDROID}
  DocDir := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
  BASS_PluginLoad(PChar(DocDir + 'libbassflac.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbassmidi.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbassopus.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbasswv.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbass_aac.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbass_alac.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbass_ape.so'), BASS_UNICODE);
  BASS_PluginLoad(PChar(DocDir + 'libbass_mpc.so'), BASS_UNICODE);

{$ELSE}
  DocDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$ENDIF}
  Result := True;
end;

function GetBASSFileInfo(filename: String; var Info: BASS_CHANNELINFO; var PlayTime: Int64): boolean;
var
  channel: Cardinal;
  PlayTimeSeconds: DOUBLE;
begin
  FillChar(Info, sizeOf(BASS_CHANNELINFO), 0);
  PlayTime := 0;
  channel := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_UNICODE OR BASS_STREAM_DECODE);
  if channel = 0 then
  begin
    channel := BASS_MusicLoad(False, PChar(filename), 0, 0, BASS_UNICODE OR BASS_STREAM_DECODE OR BASS_MUSIC_NOSAMPLE, 44100);
  end;
  if channel = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := BASS_ChannelGetInfo(channel, Info);
  PlayTimeSeconds := BASS_ChannelBytes2Seconds(channel, BASS_ChannelGetLength(channel, BASS_POS_BYTE));
  PlayTime := Round(PlayTimeSeconds * 1000);
  BASS_MusicFree(channel);
  BASS_StreamFree(channel);
end;

function GetBASSStreamType(ctype: Cardinal): String;
begin
  case ctype of
    BASS_CTYPE_STREAM_OGG:
      Result := 'Ogg Vorbis';
    BASS_CTYPE_STREAM_MP1:
      Result := 'MP1 Audio Stream';
    BASS_CTYPE_STREAM_MP2:
      Result := 'MP2 Audio Stream';
    BASS_CTYPE_STREAM_MP3:
      Result := 'MP3 Audio Stream';
    BASS_CTYPE_STREAM_AIFF:
      Result := 'Apple AIFF';
    BASS_CTYPE_STREAM_WAV:
      Result := 'WAV Audio Stream';
    BASS_CTYPE_STREAM_WAV_PCM:
      Result := 'WAV Audio Stream';
    BASS_CTYPE_STREAM_WAV_FLOAT:
      Result := 'WAV Audio Stream';
    BASS_CTYPE_MUSIC_MOD:
      Result := 'Music Module';
    BASS_CTYPE_MUSIC_MTM:
      Result := 'MTM Music Module';
    BASS_CTYPE_MUSIC_S3M:
      Result := 'S3M Music Module';
    BASS_CTYPE_MUSIC_XM:
      Result := 'XM Music Module';
    BASS_CTYPE_MUSIC_IT:
      Result := 'IT Music Module';
    BASS_CTYPE_MUSIC_MO3:
      Result := 'MO3 Music Module';
    BASS_CTYPE_STREAM_AAC:
      Result := 'AAC Audio Stream';
    BASS_CTYPE_STREAM_MP4:
      Result := 'MP4 Audio Stream';
    BASS_CTYPE_STREAM_ALAC:
      Result := 'Apple Lossless';
    BASS_CTYPE_STREAM_APE:
      Result := 'Monkey''s Audio';
    BASS_CTYPE_STREAM_MPC:
      Result := 'Musepack';
    BASS_CTYPE_STREAM_FLAC:
      Result := 'Flac Audio Stream';
    BASS_CTYPE_STREAM_FLAC_OGG:
      Result := 'Ogg Flac Audio Stream';
    BASS_CTYPE_STREAM_MIDI:
      Result := 'MIDI';
    BASS_CTYPE_STREAM_OPUS:
      Result := 'Opus Audio Codec';
    BASS_CTYPE_STREAM_WV:
      Result := 'WavPack';
  else
    Result := 'Unknown stream format';
  end;
end;

function PByte2String(Bytes: PByte): String;
begin
  Result := '';
  repeat
    Result := Result + Char(Bytes^);
    Inc(Bytes);
  until Bytes^ = 0;
end;

function BASS_BFX_CHANNEL_N(n: DWORD): DWORD;
begin
  Result := 1 shl (n - 1);
end;

end.
