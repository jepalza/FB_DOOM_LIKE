 #define T_04_HEIGHT 16
#define T_04_WIDTH 16

'' array size is 768
Dim Shared As Byte T_04(...)  = { _
  38, 43, 68, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 139, 155, 180, 38, 43, 68, 139, 155, 180, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, _
  38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, _
  139, 155, 180, 139, 155, 180, 139, 155, 180, 38, 43, 68, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, _
  180, 38, 43, 68, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, _
  90, 105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, _
  180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, _
  90, 105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, _
  180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, _
  38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, _
  38, 43, 68, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 139, 155, 180, 38, 43, 68, 139, 155, 180, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, _
  38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, 180, _
  38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, _
  139, 155, 180, 139, 155, 180, 139, 155, 180, 38, 43, 68, 139, 155, 180, 139, _
  155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, _
  180, 38, 43, 68, 139, 155, 180, 139, 155, 180, 139, 155, 180, 139, 155, 180, _
  90, 105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, _
  180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, _
  90, 105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 139, 155, _
  180, 38, 43, 68, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, _
  38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, 43, 68 _
} 