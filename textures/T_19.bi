 #define T_19_HEIGHT 16
#define T_19_WIDTH 16

'' array size is 768
Dim Shared As Byte T_19(...)  = { _
  139, 155, 180, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 139, 155, 180, 139, 155, 180, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 139, 155, 180, _
  192, 203, 220, 90, 105, 136, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 192, 203, 220, 192, 203, 220, 90, 105, 136, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 192, 203, 220, _
  192, 203, 220, 139, 155, 180, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, _
  203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 139, 155, 180, 192, 203, _
  220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, _
  192, 203, 220, 90, 105, 136, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 192, 203, 220, 192, 203, 220, 90, 105, 136, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 192, 203, 220, _
  139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 139, 155, 180, 38, 43, 68, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 139, 155, 180, 38, 43, 68, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  90, 105, 136, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 90, 105, 136, 90, 105, 136, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 90, 105, 136, _
  90, 105, 136, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 90, 105, 136, 90, 105, 136, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 90, 105, 136, _
  139, 155, 180, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 139, 155, 180, 139, 155, 180, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 139, 155, 180, _
  192, 203, 220, 90, 105, 136, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 192, 203, 220, 192, 203, 220, 90, 105, 136, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 192, 203, 220, _
  192, 203, 220, 139, 155, 180, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, _
  203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 139, 155, 180, 192, 203, _
  220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, 192, 203, 220, _
  192, 203, 220, 90, 105, 136, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 192, 203, 220, 192, 203, 220, 90, 105, 136, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 192, 203, 220, _
  139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 139, 155, 180, 38, 43, 68, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 139, 155, 180, 38, 43, 68, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  90, 105, 136, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 90, 105, 136, 90, 105, 136, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 90, 105, 136, _
  90, 105, 136, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 90, 105, 136, 90, 105, 136, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 90, 105, 136 _
} 
