 #define T_16_HEIGHT 16
#define T_16_WIDTH 16

'' array size is 768
Dim Shared As Byte T_16(...)  = { _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 120, _
  68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, _
  60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, _
  128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, 60, 128, 78, 68, 105, _
  61, 58, 120, 68, 60, 128, 78, 68, 105, 61, 58, 120, 68, 60, 120, 68, _
  60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 128, 78, 68, 120, _
  68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, _
  60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 128, 78, 68, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, _
  60, 128, 78, 68, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, _
  120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, _
  60, 128, 78, 68, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, _
  68, 60, 128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, _
  60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, _
  128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, _
  68, 60, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, _
  128, 78, 68, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, _
  68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, _
  60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, _
  105, 61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, _
  68, 60, 128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, _
  60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, _
  120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, _
  61, 58, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, 60, _
  120, 68, 60, 105, 61, 58, 120, 68, 60, 128, 78, 68, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 105, 61, _
  58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 128, 78, 68, 120, 68, 60, _
  120, 68, 60, 120, 68, 60, 120, 68, 60, 128, 78, 68, 120, 68, 60, 105, _
  61, 58, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, 60, 120, 68, _
  60, 128, 78, 68, 120, 68, 60, 105, 61, 58, 120, 68, 60, 128, 78, 68, _
  120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, _
  68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60, 105, 61, 58, 120, 68, _
  60, 128, 78, 68, 120, 68, 60, 120, 68, 60, 120, 68, 60, 120, 68, 60 _
} 