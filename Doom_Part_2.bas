' conversion desde C a FreeBasic por
' Joseba Epalza, feb. 2024 <jepalza> (gmail com)
'
' original por 3DSAGE


#Define M_PI 3.1415926535

#Include "gl/glut.bi"
#Include "windows.bi"

#define res        1                        '0=160x120 1=360x240 4=640x480
#define SW         160*res                  'screen width
#define SH         120*res                  'screen height
#define SW2        (SW/2)                   'half of screen width
#define SH2        (SH/2)                   'half of screen height
#define pixelScale 4/res                    'OpenGL pixel scale
#define GLSW       (SW*pixelScale)          'OpenGL window width
#define GLSH       (SH*pixelScale)          'OpenGL window height

'textures
#include "textures/T_NUMBERS.bi"
#include "textures/T_VIEW2D.bi"

#include "textures/T_00.bi"
#include "textures/T_01.bi"
#include "textures/T_02.bi"
#include "textures/T_03.bi"
#include "textures/T_04.bi"
#include "textures/T_05.bi"
#include "textures/T_06.bi"
#include "textures/T_07.bi"
#include "textures/T_08.bi"
#include "textures/T_09.bi"
#include "textures/T_10.bi"
#include "textures/T_11.bi"
#include "textures/T_12.bi"
#include "textures/T_13.bi"
#include "textures/T_14.bi"
#include "textures/T_15.bi"
#include "textures/T_16.bi"
#include "textures/T_17.bi"
#include "textures/T_18.bi"
#include "textures/T_19.bi"

'Dim Shared As Integer numText=19                           'number of textures
Dim Shared As Integer numSect= 0                           'number of sectors
Dim Shared As Integer numWall= 0                           'number of walls

'------------------------------------------------------------------------------
Type time_t 
 	As Integer fr1,fr2            'frame 1 frame 2, to create constant frame rate
End Type
Dim Shared As time_t TT 

Type keys 
	 As Integer w,s,a,d            'move up, down, left, rigth
	 As Integer sl,sr              'strafe left, right
	 As Integer m                  'move up, down, look up, down
End Type 
Dim Shared As keys KK 

Type math 
	 As Single cos_t(359)         'Save sin_t cos_t in values 0-360 degrees
	 As Single sin_t(359) 
End Type 
Dim Shared As math MM 

Type player 
	 As Integer x,y,z              'player position. Z is up
	 As Integer a                  'player angle of rotation left right
	 As Integer l                  'variable to look up and down
End Type 
Dim Shared As player PP 

Type walls 
	 As Integer x1,y1              'bottom line point 1
	 As Integer x2,y2              'bottom line point 2
	 As Integer c                  'wall color
	 As Integer wt,u,v             'wall texture and u/v tile
	 As Integer shade              'shade of the wall
End Type 
Dim Shared As walls WW(255) 

Type sectors 
	 As Integer ws,we              'wall number start and end
	 As Integer z1,z2              'height of bottom and top
	 As Integer d                  'add y distances to sort drawing order
	 As Integer c1,c2              'bottom and top color
	 As Integer st,ss              'surface texture, surface scale
	 As Integer surf(SW-1)           'to hold points for surfaces
	 As Integer surface            'is there a surfaces to draw
End Type 
Dim Shared As sectors SC(127) 

Type TexureMaps 
	 As Integer w,h                              'texture width/height
	 As ubyte Ptr names            'texture name
End Type 
Dim Shared As TexureMaps Textures(63)  'increase for more textures

'------------------------------------------------------------------------------

Sub load()
	
	 Dim As Integer fp = FreeFile()
	 open "level.h" For Input As fp
	 
	 If fp=0 Then 
	  Print "Error opening level.h": End 
	 EndIf
	
	 Dim As Integer s,w 
	
	 Input #fp,numSect    'number of sectors
	
	 for s=0 To numSect-1            'load all sectors
	  
		  Input #fp,SC(s).ws 
		  Input #fp,SC(s).we 
		  Input #fp,SC(s).z1 
		  Input #fp,SC(s).z2
		  Input #fp,SC(s).st 
		  Input #fp,SC(s).ss

	 Next
	 
	 
	 Input #fp,numWall    'number of walls
	
	 for s=0 To numWall-1             'load all walls
	  
		  Input #fp,WW(s).x1 
		  Input #fp,WW(s).y1 
		  Input #fp,WW(s).x2 
		  Input #fp,WW(s).y2 
		  Input #fp,WW(s).wt 
		  Input #fp,WW(s).u
		  Input #fp,WW(s).v 
		  Input #fp,WW(s).shade 
	
	 Next
	 Input #fp,s ' linea vacia, me la salto
	 
	 Input #fp,PP.x,PP.y,PP.z,PP.a,PP.l  'player position, angle, look direction
	
	 Close fp 
End Sub

Sub drawPixel(x As Integer , y As Integer , r As Integer , g As Integer , b As Integer)  'draw a pixel at x/y with rgb
	 glColor3ub(r,g,b) 
	 glBegin(GL_POINTS) 
	 glVertex2i(x*pixelScale+2,y*pixelScale+2) 
	 glEnd() 
End Sub

Sub movePlayer()
	
	'move up, down, left, right
	If(KK.a =1) AndAlso (KK.m=0) Then 
	  PP.a-=4
	  if(PP.a<  0) Then PP.a+=360 
	EndIf
	If(KK.d =1) AndAlso (KK.m=0) Then 
	  PP.a+=4
	  if(PP.a>359) Then PP.a-=360 
	EndIf
	Dim As Integer dx=MM.sin_t(PP.a)*10.0 
	Dim As Integer dy=MM.cos_t(PP.a)*10.0 
	If(KK.w =1) AndAlso (KK.m=0) Then PP.x+=dx: PP.y+=dy 
	If(KK.s =1) AndAlso (KK.m=0) Then PP.x-=dx: PP.y-=dy 
	  
	'strafe left, right
	If(KK.sr=1) Then PP.x+=dy: PP.y-=dx 
	If(KK.sl=1) Then PP.x-=dy: PP.y+=dx 
	  
	'move up, down, look up, look down
	If(KK.a=1) AndAlso (KK.m=1) Then PP.l-=1 
	If(KK.d=1) AndAlso (KK.m=1) Then PP.l+=1 
	If(KK.w=1) AndAlso (KK.m=1) Then PP.z-=4 
	If(KK.s=1) AndAlso (KK.m=1) Then PP.z+=4 
  
End Sub

Sub clearBackground()
	Dim As Integer x,y 
	For y=0 To SH-1       
	  for x=0 To SW-1 
	  		drawPixel(x,y,0,60,130) 'clear background color
	  Next
	Next
End Sub

Sub clipBehindPlayer( x1 As Integer Ptr , y1 As Integer Ptr , z1 As Integer Ptr , x2 As Integer , y2 As Integer , z2 As Integer) 'clip line
	 Dim As Single da=*y1         'distance plane -> point a
	 Dim As Single db= y2         'distance plane -> point b
	 Dim As Single d=da-db
	 If d=0 Then d=1 
	 Dim As Single s = da/(da-db) 'intersection factor (between 0 and 1)
	 *x1 = *x1 + s*(x2-(*x1)) 
	 *y1 = *y1 + s*(y2-(*y1))
	 If (*y1=0) Then *y1=1  'prevent divide by zero
	 *z1 = *z1 + s*(z2-(*z1)) 
End Sub

Sub drawWall(x1 As Integer , x2 As Integer , b1 As Integer , b2 As Integer , t1 As Integer , t2 As Integer , s As Integer , w As Integer , frontBack As Integer)

	Dim As Integer x,y 
	
	' wall texture
	Dim As Integer wt=WW(w).wt 
	
	' horizontal wall texture starting and step value
	Dim As Single ht=0, ht_step=CSng(Textures(wt).w)*WW(w).u/CSng(x2-x1) 
	
	'Hold difference in distance
	Dim As Integer dyb = b2-b1                        'y distance of bottom line
	Dim As Integer dyt = t2-t1                        'y distance of top    line
	Dim As Integer dx  = x2-x1
	If dx=0 Then dx=1   'x distance
	
	Dim As Integer xs=x1                               'hold initial x1 starting position
	
	'CLIP X
	if(x1< 0) Then ht-=ht_step*x1: x1 = 0 'clip left
	if(x2< 0) Then x2= 0 'clip left
	if(x1>SW) Then x1=SW 'clip right
	if(x2>SW) Then x2=SW 'clip right
	
	'draw x verticle lines
	for x=x1 To x2-1       
		
		'The Y start and end point
		Dim As Integer y1 = dyb*(x-xs+0.5)/dx+b1  'y bottom point
		Dim As Integer y2 = dyt*(x-xs+0.5)/dx+t1  'y bottom point
		
		' vertical wall texture starting and step value
		Dim As Single vt=0, vt_step=CSng(Textures(wt).h)*WW(w).v/CSng(y2-y1) 
		
		'Clip Y
		if(y1< 0) Then vt-=vt_step*y1: y1= 0 
		if(y2< 0) Then y2=0 
		if(y1>SH) Then y1=SH 
		if(y2>SH) Then y2=SH 
		
		
		
		'draw front wall
		If frontBack=0 Then 
		
			if(SC(s).surface=1) Then SC(s).surf(x)=y1  ' bottom surface save top row
			if(SC(s).surface=2) Then SC(s).surf(x)=y2  ' top    surface save top row
			for y=y1 To y2-1  'normal wall
				
				Dim As Integer pixel=int(Textures(wt).h-(int(vt) mod Textures(wt).h) -1)*3*Textures(wt).w + (int(ht) Mod Textures(wt).w)*3 
				Dim As Integer r=Textures(wt).names[pixel+0]-WW(w).shade/2:If(r<0) Then r=0
				Dim As Integer g=Textures(wt).names[pixel+1]-WW(w).shade/2:If(g<0) Then g=0
				Dim As Integer b=Textures(wt).names[pixel+2]-WW(w).shade/2:If(b<0) Then b=0
				
				drawPixel(x,y,r,g,b) 
				vt+=vt_step 
				
			Next
			ht+=ht_step 
		
		EndIf
		
		
		
		If frontBack=1 Then 
			
			Dim As Integer xo=SW/2  ' x offset
			Dim As Integer yo=SH/2  ' y offset
			Dim As Single  fov=200 
			Dim As Integer x2=x-xo  ' x-x offset
			Dim As Integer wo  ' wall offset
			Dim As Single tile=SC(s).ss*7  ' imported surface tile
			
			if(SC(s).surface=1) Then y2=SC(s).surf(x): wo=SC(s).z1 ' bottom surface
			if(SC(s).surface=2) Then y1=SC(s).surf(x): wo=SC(s).z2 ' top surface
			
			Dim As Single lookUpDown=-PP.l*6.2
			If lookUpDown>SH Then lookUpDown=SH 
			
			Dim As Single moveUpDown=csng(PP.z-wo)/CSng(yo)
			If moveUpDown=0 Then moveUpDown=0.001 
			
			Dim As Integer ys=y1-yo ' y start
			Dim As Integer ye=y2-yo ' y end
			
			for y=ys To ye-1       
			
				Dim As Single z = y+lookUpDown
				If (z=0) Then z=0.0001 
				
				Dim As Single fx= x2/z*moveUpDown*tile  ' world floor x
				Dim As Single fy=fov/z*moveUpDown*tile  ' world floor y
				Dim As Single rx=fx*MM.sin_t(PP.a)-fy*MM.cos_t(PP.a)+(PP.y/60.0*tile)  ' rotated texture x
				Dim As Single ry=fx*MM.cos_t(PP.a)+fy*MM.sin_t(PP.a)-(PP.x/60.0*tile)  ' rotated texture y
				If (rx<0) Then rx=-rx+1  ' remove negative values
				If (ry<0) Then ry=-ry+1  ' remove negative values

				Dim As Integer st=SC(s).st  ' surface texture
				Dim As Integer pixel=int(Textures(st).h- (int(ry) mod Textures(st).h) -1)*3*Textures(st).w + (int(rx) Mod Textures(st).w)*3 
				Dim As Integer r=Textures(st).names[pixel+0] 
				Dim As Integer g=Textures(st).names[pixel+1] 
				Dim As Integer b=Textures(st).names[pixel+2] 
				drawPixel(x2+xo,y+yo,r,g,b) 
			
			Next
			
		EndIf
	
	Next
	
End Sub

Function dist(x1 As Integer , y1 As Integer , x2 As Integer , y2 As Integer) As Integer
	Dim As Integer distance = Sqr( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) ) 
	return distance 
End Function

Sub draw3D()
	Dim As Integer x,s,w,frontBack, cycles, wx(3),wy(3),wz(3)
	Dim As Single CS=MM.cos_t(PP.a), SN=MM.sin_t(PP.a) 
	
	'order sectors by distance
	For s=0 To numSect-2       
	  for w=0 To numSect-s-2       
	   If SC(w).d<SC(w+1).d Then 
		    Dim As sectors st=SC(w)
		    SC(w)=SC(w+1)
		    SC(w+1)=st  
		EndIf
	  Next
	Next

 	'draw sectors
 	For s=0 To numSect-1       
		  
		   SC(s).d=0  'clear distance
		   If (PP.z<SC(s).z1) Then  
		  		'bottom surface
	       	SC(s).surface=1
	       	cycles=2
	       	for x=0 To SW-1
	       		SC(s).surf(x)=SH 
	       	Next
		   ElseIf (PP.z>SC(s).z2) Then
				'top surface
			   SC(s).surface=2
			   cycles=2
			   for x=0 To SW-1         
			   	SC(s).surf(x)= 0 
			   Next
		   Else
		     SC(s).surface=0
		     cycles=1  
			EndIf
			
		   'no surfaces
		   For frontBack=0 To cycles-1       
			   
			   For w=SC(s).ws To SC(s).we-1       
				    'offset bottom 2 points by player
				    Dim As Integer x1=WW(w).x1-PP.x, y1=WW(w).y1-PP.y 
				    Dim As Integer x2=WW(w).x2-PP.x, y2=WW(w).y2-PP.y 
				    'swap for surface
				    If(frontBack=1) Then 
				   	Dim As Integer swp=x1: x1=x2: x2=swp: swp=y1: y1=y2: y2=swp 
				    EndIf
				  
				    'world X position
				    wx(0)=x1*CS-y1*SN 
				    wx(1)=x2*CS-y2*SN 
				    wx(2)=wx(0)                           'top line has the same x
				    wx(3)=wx(1) 
				    'world Y position (depth)
				    wy(0)=y1*CS+x1*SN 
				    wy(1)=y2*CS+x2*SN 
				    wy(2)=wy(0)                           'top line has the same y
				    wy(3)=wy(1) 
				    SC(s).d+=dist(0,0, (wx(0)+wx(1))/2, (wy(0)+wy(1))/2 )   'store this wall distance
				    'world z height
				    wz(0)=SC(s).z1-PP.z+((PP.l*wy(0))/32.0) 
				    wz(1)=SC(s).z1-PP.z+((PP.l*wy(1))/32.0) 
				    wz(2)=SC(s).z2-PP.z+((PP.l*wy(0))/32.0) 
				    wz(3)=SC(s).z2-PP.z+((PP.l*wy(1))/32.0) 
				    'dont draw if behind player
				    If (wy(0)<1) AndAlso (wy(1)<1) Then Continue For 'wall behind player, dont draw
				    
				    'point 1 behind player, clip
				    If wy(0)<1 Then 
				     	clipBehindPlayer(@wx(0),@wy(0),@wz(0), wx(1),wy(1),wz(1))  'bottom line
				     	clipBehindPlayer(@wx(2),@wy(2),@wz(2), wx(3),wy(3),wz(3))  'top line
				    EndIf
				  
				    'point 2 behind player, clip
				    If wy(1)<1 Then 
				     	clipBehindPlayer(@wx(1),@wy(1),@wz(1), wx(0),wy(0),wz(0))  'bottom line
				     	clipBehindPlayer(@wx(3),@wy(3),@wz(3), wx(2),wy(2),wz(2))  'top line
				    EndIf
				  
				    'screen x, screen y position
				    wx(0)=wx(0)*200/wy(0)+SW2: wy(0)=wz(0)*200/wy(0)+SH2 
				    wx(1)=wx(1)*200/wy(1)+SW2: wy(1)=wz(1)*200/wy(1)+SH2 
				    wx(2)=wx(2)*200/wy(2)+SW2: wy(2)=wz(2)*200/wy(2)+SH2 
				    wx(3)=wx(3)*200/wy(3)+SW2: wy(3)=wz(3)*200/wy(3)+SH2 
				    'draw points
				    drawWall(wx(0),wx(1), wy(0),wy(1), wy(2),wy(3), s,w, frontBack) 
			   
			   Next
			   SC(s).d/=(SC(s).we-SC(s).ws)  'find average sector distance
			  
		   Next
 	Next
End Sub


Sub testTextures(t As Integer)
	Dim As Integer x,y 

	for y=0 To Textures(t).h-1       
		for x=0 To Textures(t).w-1       
			Dim As Integer pixel=(Textures(t).h-y-1)*3*Textures(t).w + x*3 
			Dim As Integer r=Textures(t).names[pixel+0] 
			Dim As Integer g=Textures(t).names[pixel+1] 
			Dim As Integer b=Textures(t).names[pixel+2] 
			drawPixel(x,y,r,g,b) 
		Next
	Next
	
End Sub

Sub testFloors()

	Dim As Integer x,y 
	Dim As Integer xo=SW/2  ' x offset
	Dim As Integer yo=SH/2  ' y offset
	Dim As Single fov=200 
	Dim As Single lookUpDown=-PP.l*4
	if(lookUpDown>SH) Then lookUpDown=SH 
  
	Dim As Single moveUpDown=PP.z/16.0
	if(moveUpDown=0) Then moveUpDown=0.001 
  
	Dim As Integer ys=-yo, ye=-lookUpDown 
	if(moveUpDown<0) Then ys=-lookUpDown: ye=yo+lookUpDown 
  
	for y=ys To ye -1      
		for x=-xo To xo-1       
			Dim As Single z = y+lookUpDown
			if(z=0) Then z=0.0001 
  
			Dim As Single fx=  x/z*moveUpDown  ' world floor x
			Dim As Single fy=fov/z*moveUpDown  ' world floor y
			Dim As Single rx=fx*MM.sin_t(PP.a)-fy*MM.cos_t(PP.a)+(PP.y/30.0)  ' rotated texture x
			Dim As Single ry=fx*MM.cos_t(PP.a)+fy*MM.sin_t(PP.a)-(PP.x/30.0)  ' rotated texture y
			if(rx<0) Then rx=-rx+1 ' remove negative values
			if(ry<0) Then ry=-ry+1 ' remove negative values
			if(rx<=0 OrElse ry<=0 OrElse rx>=5 OrElse ry>=5) Then Continue For

  
			' suelo que se difumina en la niebla
			' int c=255*(255-(y/yo));
			' if(rxmod2==rymod2){ drawPixel(x+xo,y+yo, c,  60,130);}
			' else                    { drawPixel(x+xo,y+yo,  0 ,60,130);}
		
		Next
	Next
End Sub

Sub display Cdecl ()
	 Dim As Integer x,y 
	 if(TT.fr1-TT.fr2>=50) Then 'only draw 20 frames/second
	  
		  clearBackground() 
		  movePlayer() 
		
		  'testTextures(4);  // para ver los bloques de textura
		  'testFloors(); // para verlos suelos tipo ajedrez
		  draw3D() 
		
		  TT.fr2=TT.fr1 
		  glutSwapBuffers() 
		  glutReshapeWindow(GLSW,GLSH)              'prevent window scaling
	 
	 EndIf
	
	 TT.fr1=glutGet(GLUT_ELAPSED_TIME)           '1000 Milliseconds per second
	 glutPostRedisplay() 
End Sub

Sub KeysDown Cdecl (ByVal key As Integer , ByVal x As Integer , ByVal y As Integer)
	key=key And 255.
	 If key=Asc("w") Then KK.w =1 
	 If key=Asc("s") Then KK.s =1 
	 If key=Asc("a") Then KK.a =1 
	 If key=Asc("d") Then KK.d =1 
	 If key=Asc("m") Then KK.m =1 
	 If key=Asc(",") Then KK.sr=1 
	 If key=Asc(".") Then KK.sl=1 
	 If key=13 Then load() ' enter key, load level
End Sub

Sub KeysUp Cdecl (ByVal key As Integer ,byval x As Integer ,ByVal y As Integer)
	key=key And 255
	 If key=Asc("w") Then KK.w =0 
	 If key=Asc("s") Then KK.s =0 
	 If key=Asc("a") Then KK.a =0 
	 If key=Asc("d") Then KK.d =0 
	 If key=Asc("m") Then KK.m =0 
	 If key=Asc(",") Then KK.sr=0 
	 If key=Asc(".") Then KK.sl=0 
End Sub

Sub init()
	Dim As Integer x 
	 'store sin_t/cos_t in degrees
	 for x=0 To 360-1 'precalulate sin_t cos_t in degrees
	  	MM.cos_t(x)=Cos(x/180.0*M_PI) 
	  	MM.sin_t(x)=sin(x/180.0*M_PI) 
	 Next
	 
	 'init player
	 PP.x=70: PP.y=-110: PP.z=20: PP.a=0: PP.l=0     'init player variables
	
	 ' define textures
	 Textures( 0).names=@T_00(0): Textures( 0).h=T_00_HEIGHT: Textures( 0).w=T_00_WIDTH 
	 Textures( 1).names=@T_01(0): Textures( 1).h=T_01_HEIGHT: Textures( 1).w=T_01_WIDTH 
	 Textures( 2).names=@T_02(0): Textures( 2).h=T_02_HEIGHT: Textures( 2).w=T_02_WIDTH 
	 Textures( 3).names=@T_03(0): Textures( 3).h=T_03_HEIGHT: Textures( 3).w=T_03_WIDTH 
	 Textures( 4).names=@T_04(0): Textures( 4).h=T_04_HEIGHT: Textures( 4).w=T_04_WIDTH 
	 Textures( 5).names=@T_05(0): Textures( 5).h=T_05_HEIGHT: Textures( 5).w=T_05_WIDTH 
	 Textures( 6).names=@T_06(0): Textures( 6).h=T_06_HEIGHT: Textures( 6).w=T_06_WIDTH 
	 Textures( 7).names=@T_07(0): Textures( 7).h=T_07_HEIGHT: Textures( 7).w=T_07_WIDTH 
	 Textures( 8).names=@T_08(0): Textures( 8).h=T_08_HEIGHT: Textures( 8).w=T_08_WIDTH 
	 Textures( 9).names=@T_09(0): Textures( 9).h=T_09_HEIGHT: Textures( 9).w=T_09_WIDTH 
	 Textures(10).names=@T_10(0): Textures(10).h=T_10_HEIGHT: Textures(10).w=T_10_WIDTH 
	 Textures(11).names=@T_11(0): Textures(11).h=T_11_HEIGHT: Textures(11).w=T_11_WIDTH 
	 Textures(12).names=@T_12(0): Textures(12).h=T_12_HEIGHT: Textures(12).w=T_12_WIDTH 
	 Textures(13).names=@T_13(0): Textures(13).h=T_13_HEIGHT: Textures(13).w=T_13_WIDTH 
	 Textures(14).names=@T_14(0): Textures(14).h=T_14_HEIGHT: Textures(14).w=T_14_WIDTH 
	 Textures(15).names=@T_15(0): Textures(15).h=T_15_HEIGHT: Textures(15).w=T_15_WIDTH 
	 Textures(16).names=@T_16(0): Textures(16).h=T_16_HEIGHT: Textures(16).w=T_16_WIDTH 
	 Textures(17).names=@T_17(0): Textures(17).h=T_17_HEIGHT: Textures(17).w=T_17_WIDTH 
	 Textures(18).names=@T_18(0): Textures(18).h=T_18_HEIGHT: Textures(18).w=T_18_WIDTH 
	 Textures(19).names=@T_19(0): Textures(19).h=T_19_HEIGHT: Textures(19).w=T_19_WIDTH 
End Sub





 'glutInit(@argc, argv) 
 glutInitDisplayMode(GLUT_DOUBLE Or GLUT_RGB) 
 glutInitWindowPosition(GLSW/2,GLSH/2) 
 glutInitWindowSize(GLSW,GLSH) 
 glutCreateWindow("") 
 glPointSize(pixelScale) 'pixel size
 gluOrtho2D(0,GLSW,0,GLSH) 'origin bottom left
 init()
 glutDisplayFunc(@display) 
 glutKeyboardFunc(@KeysDown) 
 glutKeyboardUpFunc(@KeysUp) 
 glutMainLoop() 


