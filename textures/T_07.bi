 #define T_07_HEIGHT 16
#define T_07_WIDTH 16

'' array size is 768
Dim Shared As Byte T_07(...)  = { _
  69, 81, 113, 69, 81, 113, 90, 105, 136, 90, 105, 136, 90, 105, 136, 114, _
  130, 157, 90, 105, 136, 90, 105, 136, 90, 105, 136, 69, 81, 113, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 69, 81, 113, 77, 89, 121, _
  69, 81, 113, 79, 92, 123, 58, 68, 102, 58, 68, 102, 58, 68, 102, 58, _
  68, 102, 58, 68, 102, 58, 68, 102, 58, 68, 102, 38, 43, 68, 38, 43, _
  68, 38, 43, 68, 38, 43, 68, 38, 43, 68, 79, 92, 123, 69, 81, 113, _
  90, 105, 136, 58, 68, 102, 91, 106, 136, 139, 155, 180, 139, 155, 180, 118, _
  134, 161, 118, 134, 161, 118, 134, 161, 139, 155, 180, 139, 155, 180, 139, 155, _
  180, 139, 155, 180, 139, 155, 180, 91, 106, 136, 38, 43, 68, 90, 105, 136, _
  90, 105, 136, 58, 68, 102, 73, 85, 117, 114, 130, 157, 114, 130, 157, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 139, 155, 180, 38, 43, 68, 90, 105, 136, _
  114, 130, 157, 58, 68, 102, 73, 85, 117, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 114, 130, 157, 139, 155, 180, 38, 43, 68, 90, 105, 136, _
  90, 105, 136, 58, 68, 102, 73, 85, 117, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 116, 132, 160, 116, 132, 160, 116, 132, 160, 116, 132, 160, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 139, 155, 180, 58, 68, 102, 69, 81, 113, _
  90, 105, 136, 58, 68, 102, 73, 85, 117, 90, 105, 136, 90, 105, 136, 139, _
  155, 180, 58, 68, 102, 58, 68, 102, 38, 43, 68, 38, 43, 68, 116, 132, _
  160, 90, 105, 136, 90, 105, 136, 139, 155, 180, 58, 68, 102, 90, 105, 136, _
  69, 81, 113, 58, 68, 102, 73, 85, 117, 114, 130, 157, 90, 105, 136, 139, _
  155, 180, 79, 92, 123, 90, 105, 136, 90, 105, 136, 38, 43, 68, 116, 132, _
  160, 90, 105, 136, 90, 105, 136, 139, 155, 180, 58, 68, 102, 114, 130, 157, _
  90, 105, 136, 38, 43, 68, 73, 85, 117, 90, 105, 136, 90, 105, 136, 139, _
  155, 180, 79, 92, 123, 90, 105, 136, 90, 105, 136, 38, 43, 68, 139, 155, _
  180, 90, 105, 136, 90, 105, 136, 118, 134, 161, 58, 68, 102, 90, 105, 136, _
  90, 105, 136, 38, 43, 68, 73, 85, 117, 90, 105, 136, 90, 105, 136, 139, _
  155, 180, 79, 92, 123, 79, 92, 123, 79, 92, 123, 58, 68, 102, 139, 155, _
  180, 90, 105, 136, 90, 105, 136, 118, 134, 161, 58, 68, 102, 90, 105, 136, _
  114, 130, 157, 38, 43, 68, 73, 85, 117, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 139, 155, 180, 116, 132, 160, 139, 155, 180, 139, 155, 180, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 118, 134, 161, 58, 68, 102, 90, 105, 136, _
  114, 130, 157, 38, 43, 68, 73, 85, 117, 114, 130, 157, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, _
  136, 90, 105, 136, 90, 105, 136, 139, 155, 180, 58, 68, 102, 90, 105, 136, _
  90, 105, 136, 38, 43, 68, 73, 85, 117, 90, 105, 136, 90, 105, 136, 114, _
  130, 157, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, 105, 136, 114, 130, _
  157, 114, 130, 157, 90, 105, 136, 118, 134, 161, 58, 68, 102, 90, 105, 136, _
  90, 105, 136, 38, 43, 68, 90, 105, 136, 73, 85, 117, 73, 85, 117, 73, _
  85, 117, 73, 85, 117, 73, 85, 117, 73, 85, 117, 73, 85, 117, 73, 85, _
  117, 73, 85, 117, 73, 85, 117, 91, 106, 136, 58, 68, 102, 90, 105, 136, _
  69, 81, 113, 79, 92, 123, 38, 43, 68, 38, 43, 68, 38, 43, 68, 38, _
  43, 68, 38, 43, 68, 38, 43, 68, 58, 68, 102, 58, 68, 102, 58, 68, _
  102, 58, 68, 102, 58, 68, 102, 58, 68, 102, 79, 92, 123, 69, 81, 113, _
  69, 81, 113, 69, 81, 113, 90, 105, 136, 90, 105, 136, 90, 105, 136, 90, _
  105, 136, 90, 105, 136, 114, 130, 157, 90, 105, 136, 90, 105, 136, 69, 81, _
  113, 90, 105, 136, 90, 105, 136, 114, 130, 157, 69, 81, 113, 69, 81, 113 _
} 
