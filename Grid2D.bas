 '--------------------------By:3DSage-------------------------------------------
'               https://www.youtube.com/3dsage
'                         version 1.0

' conversion desde C a FreeBasic por
' Joseba Epalza, feb. 2024 <jepalza> (gmail com)


#Define M_PI 3.141579

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

Dim Shared As Integer numText=19                           'number of textures
Dim Shared As Integer numSect= 0                           'number of sectors
Dim Shared As Integer numWall= 0                           'number of walls


'------------------------------------------------------------------------------

Type time_t 
 As Integer fr1,fr2            'frame 1 frame 2, to create constant frame rate
End Type 
Dim Shared As time_t TT 

Type math 
 As Single cos_t(359)         'Save sin_t cos_t in values 0-360 degrees
 As Single sin_t(359) 
 End Type 
Dim Shared As math MM 

Type keys 
 As Integer w,s,a,d            'move up, down, left, rigth
 As Integer sl,sr              'strafe left, right
 As Integer m                  'move up, down, look up, down
End Type 
Dim Shared As keys KK 

Type player 
 As Integer x,y,z              'player position. Z is up
 As Integer a                  'player angle of rotation left right
 As Integer l                  'variable to look up and down
End Type 
Dim Shared As player PP 

Type walls 
 As Integer x1,y1              'bottom line point 1
 As Integer x2,y2              'bottom line point 2
 As Integer wt,u,v             'wall texture and u/v tile
 As Integer shade              'shade of the wall
 End Type 
Dim Shared As walls WW(255) 

Type sectors 
 As Integer ws,we              'wall number start and end
 As Integer z1,z2              'height of bottom and top
 As Integer d                  'add y distances to sort drawing order
 As Integer st,ss              'surface texture, surface scale
 As Integer surf(SW)           'to hold points for surfaces
End Type 
Dim Shared As sectors SC(127) 

Type TexureMaps 
 As Integer w,h                              'texture width/height
 As ubyte Ptr names            'texture name
End Type 
Dim Shared As TexureMaps Textures(63)  'increase for more textures

Type grid 
 As Integer mx,my         'rounded mouse position
 As Integer addSect       '0=nothing, 1=add sector
 As Integer wt,wu,wv      'wall    texture, uv texture tile
 As Integer st,ss         'surface texture, surface scale
 As Integer z1,z2         'bottom and top height
 As Integer scale         'scale down grid
 As Integer move(3)       '0=wall ID, 1=v1v2, 2=wallID, 3=v1v2
 As Integer selS,selW     'select sector/wall
End Type 
Dim Shared As grid GG 

'------------------------------------------------------------------------------

Sub save() 'save file

	 Dim As Integer fp = FreeFile()
	 open "level.h" For output As fp
	 
	 If fp=0 Then 
	  Print "Error opening level.h": End 
	 EndIf
	
	 Dim As Integer s,w 

	 Print #fp,numSect    'number of sectors
	
	 for s=0 To numSect-1            'load all sectors
		  Print #fp,SC(s).ws;" ";SC(s).we;" ";SC(s).z1;" ";SC(s).z2;" ";SC(s).st;" ";SC(s).ss
	 Next
	 
	 
	 Print #fp,numWall    'number of walls
	
	 for s=0 To numWall-1             'load all walls
		  Print #fp,WW(s).x1;" ";WW(s).y1;" ";WW(s).x2;" ";WW(s).y2;" ";WW(s).wt;" ";WW(s).u;" ";WW(s).v;" ";WW(s).shade 
	 Next
	 print #fp,"" ' linea vacia
	 
	 print #fp,PP.x;" ";PP.y;" ";PP.z;" ";PP.a;" ";PP.l  'player position, angle, look direction
	
	 Close fp 
End Sub

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

Sub initGlobals() 	        'define grid globals
 GG.scale=4                  'scale down grid
 GG.selS=0: GG.selW=0        'select sector, walls
 GG.z1=0:   GG.z2=40         'sector bottom top height
 GG.st=1:   GG.ss=4          'sector texture, scale
 GG.wt=0:   GG.wu=1: GG.wv=1 'wall texture, u,v
End Sub

Sub drawPixel(x As Integer , y As Integer , r As Integer , g As Integer , b As Integer) 'draw a pixel at x/y with rgb
 glColor3ub(r,g,b) 
 glBegin(GL_POINTS) 
 glVertex2i(x*pixelScale+2,y*pixelScale+2) 
 glEnd() 
End Sub

Sub drawLine(x1 As Single , y1 As Single , x2 As Single , y2 As Single , r As Integer , g As Integer , b As Integer)
	Dim As Integer n 
	Dim As Single x=x2-x1 
	Dim As Single y=y2-y1 
	Dim As Single max_=abs(x): if(abs(y)>max_) Then max_=abs(y) 
	
	x /= max_: y /= max_ 
	for n=0 To max_-1       
		drawPixel(x1,y1,r,g,b) 
		x1+=x: y1+=y 
	Next
End Sub

Sub drawNumber(nx As Integer , ny As Integer , n As Integer)
	Dim As Integer x,y 
	for y=0 To 4      
	
	Dim As Integer y2=((5-y-1)+5*n)*3*12 
	for x=0 To 11      
		
		Dim As Integer x2=x*3 
		if(T_NUMBERS(y2+x2)=0) Then Continue For 
		
		drawPixel(x+nx,y+ny,255,255,255) 
		
	Next
	
	Next
End Sub

Sub draw2D()
	Dim As Integer s,w,x,y,c 
	
	'draw background color
	for y=0 To 119     
	
		Dim As Integer y2=(SH-y-1)*3*160  'invert height, x3 for rgb, x15 for texture width
		for x=0 To 159      
			
			Dim As Integer pixel=x*3+y2 
			Dim As Integer r=T_VIEW2D(pixel+0) 
			Dim As Integer g=T_VIEW2D(pixel+1) 
			Dim As Integer b=T_VIEW2D(pixel+2) 
			if(GG.addSect>0 AndAlso y>48-8 AndAlso y<56-8 AndAlso x>144) Then 
				r=r Shr 1: g=g Shr 1: b=b Shr 1 
			EndIf
			'darken sector button
			drawPixel(x,y,r,g,b) 
	
		Next
	
	Next

	'draw sectors
	for s=0 To numSect-1       
		for w=SC(s).ws To SC(s).we -1      
				if(s=GG.selS-1) Then  'if this sector is selected
					'set sector to globals
					SC(GG.selS-1).z1=GG.z1 
					SC(GG.selS-1).z2=GG.z2 
					SC(GG.selS-1).st=GG.st 
					SC(GG.selS-1).ss=GG.ss 
					'yellow select
					If(GG.selW=0) Then
						c=80   'all walls yellow
					ElseIf (GG.selW+SC(s).ws-1=w) Then
						c=80: WW(w).wt=GG.wt: WW(w).u=GG.wu: WW(w).v=GG.wv   'one wall selected
					else
						c= 0    'grey walls
					EndIf
				Else
					c=0 'sector not selected, grey
				EndIf
				
				drawLine (WW(w).x1/GG.scale,WW(w).y1/GG.scale, WW(w).x2/GG.scale,WW(w).y2/GG.scale,128+c,128+c,128-c) 
				drawPixel(WW(w).x1/GG.scale,WW(w).y1/GG.scale,255,255,255) 
				drawPixel(WW(w).x2/GG.scale,WW(w).y2/GG.scale,255,255,255) 
			
		Next
	Next

	'draw player
	Dim As Integer dx=MM.sin_t(PP.a)*12 
	Dim As Integer dy=MM.cos_t(PP.a)*12 
	drawPixel(PP.x/GG.scale,PP.y/GG.scale,0,255,0) 
	drawPixel((PP.x+dx)/GG.scale,(PP.y+dy)/GG.scale,0,175,0) 
	
	'draw wall texture
	Dim As Single tx=0, tx_stp=Textures(GG.wt).w/15.0 
	Dim As Single ty=0, ty_stp=Textures(GG.wt).h/15.0 
	
	for y=0 To 14      
		tx=0 
		for x=0 To 14      
			Dim As Integer x2=Int(tx) Mod Textures(GG.wt).w: tx+=tx_stp '*GG.wu;
			Dim As Integer y2=Int(ty) Mod Textures(GG.wt).h 
			Dim As Integer r=Textures(GG.wt).names[(Textures(GG.wt).h-y2-1)*3*Textures(GG.wt).w+x2*3+0]
			Dim As Integer g=Textures(GG.wt).names[(Textures(GG.wt).h-y2-1)*3*Textures(GG.wt).w+x2*3+1] 
			Dim As Integer b=Textures(GG.wt).names[(Textures(GG.wt).h-y2-1)*3*Textures(GG.wt).w+x2*3+2] 
			drawPixel(x+145,y+105-8,r,g,b) 
		Next
		ty+=ty_stp '*GG.wv;
	Next
 
	'draw surface texture
	tx=0 : tx_stp=Textures(GG.st).w/15.0 
	ty=0 : ty_stp=Textures(GG.st).h/15.0 
	for y=0 To 14       
		tx=0 
		for x=0 To 14      
			Dim As Integer x2=Int(tx) Mod Textures(GG.st).w: tx+=tx_stp '*GG.ss;
			Dim As Integer y2=Int(ty) Mod Textures(GG.st).h 
			Dim As Integer r=Textures(GG.st).names[(Textures(GG.st).h-y2-1)*3*Textures(GG.st).w+x2*3+0]
			Dim As Integer g=Textures(GG.st).names[(Textures(GG.st).h-y2-1)*3*Textures(GG.st).w+x2*3+1] 
			Dim As Integer b=Textures(GG.st).names[(Textures(GG.st).h-y2-1)*3*Textures(GG.st).w+x2*3+2] 
			drawPixel(x+145,y+105-24-8,r,g,b) 
		Next
		ty+=ty_stp '*GG.ss;
	Next
 
	 'draw numbers
	 drawNumber(140,90,GG.wu)    'wall u
	 drawNumber(148,90,GG.wv)    'wall v
	 drawNumber(148,66,GG.ss)    'surface v
	 drawNumber(148,58,GG.z2)    'top height
	 drawNumber(148,50,GG.z1)    'bottom height
	 drawNumber(148,26,GG.selS)  'sector number
	 drawNumber(148,18,GG.selW)  'wall number
End Sub

'darken buttons
Dim Shared As Integer dark=0 
Sub darken()                       'draw a pixel at x/y with rgb
	Dim As Integer x,y, xs,xe, ys,ye 
	
	 if(dark= 0) Then return  'no buttons were clicked
              
	 if(dark= 1) Then xs= 0: xe=15: ys= 0/GG.scale : ye= 32/GG.scale 'save button
	 if(dark= 2) Then xs= 0: xe= 3: ys=96/GG.scale : ye=128/GG.scale 'u left
	 if(dark= 3) Then xs= 4: xe= 8: ys=96/GG.scale : ye=128/GG.scale 'u right
	 if(dark= 4) Then xs= 7: xe=11: ys=96/GG.scale : ye=128/GG.scale 'v left
	 if(dark= 5) Then xs=11: xe=15: ys=96/GG.scale : ye=128/GG.scale 'u right
	 if(dark= 6) Then xs= 0: xe= 8: ys=192/GG.scale: ye=224/GG.scale 'u left
	 if(dark= 7) Then xs= 8: xe=15: ys=192/GG.scale: ye=224/GG.scale 'u right
	 if(dark= 8) Then xs=0 : xe= 7: ys=224/GG.scale: ye=256/GG.scale 'Top left
	 if(dark= 9) Then xs=7 : xe=15: ys=224/GG.scale: ye=256/GG.scale 'Top right
	 if(dark=10) Then xs=0 : xe= 7: ys=256/GG.scale: ye=288/GG.scale 'Bot left
	 if(dark=11) Then xs=7 : xe=15: ys=256/GG.scale: ye=288/GG.scale 'Bot right
	 if(dark=12) Then xs=0 : xe= 7: ys=352/GG.scale: ye=386/GG.scale 'sector left
	 if(dark=13) Then xs=7 : xe=15: ys=352/GG.scale: ye=386/GG.scale 'sector right
	 if(dark=14) Then xs=0 : xe= 7: ys=386/GG.scale: ye=416/GG.scale 'wall left
	 if(dark=15) Then xs=7 : xe=15: ys=386/GG.scale: ye=416/GG.scale 'wall right
	 if(dark=16) Then xs=0 : xe=15: ys=416/GG.scale: ye=448/GG.scale 'delete
	 if(dark=17) Then xs=0 : xe=15: ys=448/GG.scale: ye=480/GG.scale 'load

	for y=ys To ye-1       
		for x=xs To xe-1       
			glColor4f(0,0,0,0.4) 
			glBegin(GL_POINTS) 
			glVertex2i(x*pixelScale+2+580,(120-y)*pixelScale) 
			glEnd() 
		Next
	Next
 
End Sub

Sub mouse Cdecl (ByVal button As Integer ,BYVAL state As Integer ,byval x As Integer ,ByVal y As Integer)
	Dim As Integer s,w 
	
	'round mouse x,y
	GG.mx=x/pixelScale 
	GG.my=SH-y/pixelScale 
	GG.mx=((GG.mx+4) Shr 3) Shl 3 
	GG.my=((GG.my+4) Shr 3) Shl 3  'nearest 8th

	' ---------------------------------------------
	if(button = GLUT_LEFT_BUTTON) AndAlso (state = GLUT_DOWN) Then 
		
		'2D view buttons only
		If(x>580) Then 
				
			'2d 3d view buttons
			if(y>0 ) AndAlso (y<32) Then 
				save() : dark=1 
			EndIf
			
			'wall texture
			if(y>32) AndAlso (y<96) Then 
				if(x<610) Then 
					GG.wt-=1
					if(GG.wt<0) Then GG.wt=numText
				Else
					GG.wt+=1
					If(GG.wt>numText) Then GG.wt=0 
				EndIf
			EndIf
					   	
			'wall uv
			if(y>96 ) AndAlso (y<128) Then 
				If(x<595) Then 
					dark=2: GG.wu-=1
					if(GG.wu< 1) Then GG.wu= 1   
				ElseIf (x<610) Then
					dark=3: GG.wu+=1
					if(GG.wu> 9) Then GG.wu= 9   
				ElseIf (x<625) Then
					dark=4: GG.wv-=1
					if(GG.wv< 1) Then GG.wv= 1   
				ElseIf (x<640) Then
					dark=5: GG.wv+=1
					if(GG.wv> 9) Then GG.wv= 9
				EndIf
			EndIf
					  
			'surface texture
			if(y>128) AndAlso (y<192) Then 
				if(x<610) Then 
					GG.st-=1
					if(GG.st<0) Then GG.st=numText
				Else
					GG.st+=1
					If(GG.st>numText) Then GG.st=0 
				EndIf
			EndIf
					  
			'surface uv
			if(y>192) AndAlso (y<222) Then 
				if(x<610) Then 
					dark=6
					GG.ss-=1
					if(GG.ss< 1) Then GG.ss= 1   
				Else
					dark=7
					GG.ss+=1
					If(GG.ss> 9) Then GG.ss= 9 
				EndIf
			EndIf
					  
			'top height
			if(y>222) AndAlso (y<256) Then 
				if(x<610) Then 
					dark=8
					GG.z2-=5
					If (GG.z2=GG.z1) Then GG.z1-=5   
				Else
					dark=9
					GG.z2+=5 
				EndIf
			EndIf
					  
			'bot height
			if(y>256) AndAlso (y<288) Then 
				if(x<610) Then 
					dark=10: GG.z1-=5  
				else
					dark=11: GG.z1+=5
					If (GG.z1=GG.z2) Then GG.z2+=5 
				EndIf
			EndIf
			
			'add sector
			if(y>288) AndAlso (y<318) Then 
				GG.addSect+=1: GG.selS=0: GG.selW=0
				if(GG.addSect>1) Then GG.addSect=0 
			EndIf
					  
			'limit
			If(GG.z1<0  ) Then GG.z1=0 
			If(GG.z1>145) Then GG.z1=145 
			  
			if(GG.z2<5  ) Then GG.z2=5 
			If(GG.z2>150) Then GG.z2=150 
					  
			'select sector
			if(y>352) AndAlso (y<386) Then 
				GG.selW=0 
				if(x<610) Then 
					dark=12
					GG.selS-=1
					if(GG.selS<0) Then GG.selS=numSect   
				Else
					dark=13
					GG.selS+=1
					if(GG.selS>numSect) Then GG.selS=0 
				EndIf
				
				Dim As Integer s=GG.selS-1 
				GG.z1=SC(s).z1  'sector bottom height
				GG.z2=SC(s).z2  'sector top height
				GG.st=SC(s).st  'surface texture
				GG.ss=SC(s).ss  'surface scale
				GG.wt=WW(SC(s).ws).wt 
				GG.wu=WW(SC(s).ws).u 
				GG.wv=WW(SC(s).ws).v 
				if(GG.selS=0) Then initGlobals()'defaults 
			EndIf
					  
		   'select sector´s walls
		   Dim As Integer snw=SC(GG.selS-1).we-SC(GG.selS-1).ws  'sector´s number of walls
		   if(y>386) AndAlso (y<416) Then 
			   If(x<610) Then  'select sector wall left
			     dark=14 
			     GG.selW-=1
			     if(GG.selW<0) Then GG.selW=snw
			   Else  'select sector wall right 
			     dark=15
			     GG.selW+=1
			     if(GG.selW>snw) Then GG.selW=0 
			   EndIf
			   If(GG.selW>0) Then 
			     GG.wt=WW(SC(GG.selS-1).ws+GG.selW-1).wt  'printf("ws,%i,%i\n",GG.wt, 1 );
			     GG.wu=WW(SC(GG.selS-1).ws+GG.selW-1).u 
			     GG.wv=WW(SC(GG.selS-1).ws+GG.selW-1).v 
			   EndIf
		   EndIf
					  
		   'delete
		   if(y>416) AndAlso (y<448) Then 
			   dark=16 
			   if(GG.selS>0) Then 
				   Dim As Integer d=GG.selS-1                              'delete this one
					'printf("%i before:%i,%i\n",d, numSect,numWall);
				   numWall-=(SC(d).we-SC(d).ws)                  'first subtract number of walls
				   For x=d To numSect-1
				   	SC(x)=SC(x+1) 'remove from array
				   Next
				   numSect-=1                                  '1 less sector
				   GG.selS=0: GG.selW=0                          'deselect
				   'printf("after:%i,%i\n\n",numSect,numWall);
			   EndIf
		   EndIf
		   
		   'load
		   if(y>448) AndAlso (y<480) Then dark=17: load()  
	   
		'clicked on grid
		Else
	
			'init new sector
			if(GG.addSect=1) Then 

				SC(numSect).ws=numWall		'clear wall start
				SC(numSect).we=numWall+1   'add 1 to wall end
				SC(numSect).z1=GG.z1 
				SC(numSect).z2=GG.z2 
				SC(numSect).st=GG.st 
				SC(numSect).ss=GG.ss 
				WW(numWall).x1=GG.mx*GG.scale: WW(numWall).y1=GG.my*GG.scale   'x1,y1
				WW(numWall).x2=GG.mx*GG.scale: WW(numWall).y2=GG.my*GG.scale   'x2,y2
				WW(numWall).wt=GG.wt 
				WW(numWall).u=GG.wu 
				WW(numWall).v=GG.wv 
				numWall+=1                                               'add 1 wall
				numSect+=1                                               'add this sector
				GG.addSect=3                                              'go to point 2

			'add point 2
			ElseIf (GG.addSect=3) Then 

				if(SC(numSect-1).ws=numWall-1) AndAlso (GG.mx*GG.scale<=WW(SC(numSect-1).ws).x1) Then 
					numWall-=1: numSect-=1: GG.addSect=0 
					Print "walls must be counter clockwise"
					Exit sub 
				EndIf
				
				'point 2
				WW(numWall-1).x2=GG.mx*GG.scale: WW(numWall-1).y2=GG.my*GG.scale  'x2,y2
				
				'automatic shading
				Dim As Single ang = ATan2( WW(numWall-1).y2-WW(numWall-1).y1, WW(numWall-1).x2-WW(numWall-1).x1 ) 
				ang=(ang*180)/M_PI       'radians to degrees
				if(ang<0) Then ang+=360 'correct negative
				Dim As Integer shade=ang  'shading goes from 0-90-0-90-0
				if(shade>180) Then shade=180-(shade-180) 
				if(shade> 90) Then shade= 90-(shade- 90) 

				WW(numWall-1).shade=shade 
				
				'check if sector is closed
				If (WW(numWall-1).x2=WW(SC(numSect-1).ws).x1) AndAlso (WW(numWall-1).y2=WW(SC(numSect-1).ws).y1) Then 
					WW(numWall-1).wt=GG.wt 
					WW(numWall-1).u=GG.wu 
					WW(numWall-1).v=GG.wv 
					GG.addSect=0 
				'not closed, add new wall
				Else
					'init next wall
					SC(numSect-1).we+=1                                       'add 1 to wall end
					WW(numWall).x1=GG.mx*GG.scale: WW(numWall).y1=GG.my*GG.scale   'x1,y1
					WW(numWall).x2=GG.mx*GG.scale: WW(numWall).y2=GG.my*GG.scale   'x2,y2
					WW(numWall-1).wt=GG.wt 
					WW(numWall-1).u=GG.wu 
					WW(numWall-1).v=GG.wv 
					WW(numWall).shade=0 
					numWall+=1                                               'add 1 wall
				EndIf
			EndIf
		EndIf
		
	EndIf  ' fin de -> if(button = GLUT_LEFT_BUTTON) Then '2D view buttons only
  	'-------------------------------------------

	'clear variables to move point
	for w=0 To 3
		GG.move(w)=-1 
	Next

	if(GG.addSect=0) AndAlso (button = GLUT_RIGHT_BUTTON) AndAlso (state = GLUT_DOWN) Then 
		'move point hold id
		for s=0 To numSect-1       
			for w=SC(s).ws To SC(s).we-1       
				Dim As Integer x1=WW(w).x1, y1=WW(w).y1 
				Dim As Integer x2=WW(w).x2, y2=WW(w).y2 
				Dim As Integer mx=GG.mx*GG.scale, my=GG.my*GG.scale 
				if(mx<x1+3 AndAlso mx>x1-3 AndAlso my<y1+3 AndAlso my>y1-3) Then GG.move(0)=w: GG.move(1)=1 
				if(mx<x2+3 AndAlso mx>x2-3 AndAlso my<y2+3 AndAlso my>y2-3) Then GG.move(2)=w: GG.move(3)=2 
			Next
		Next
	EndIf
  
	if(button = GLUT_LEFT_BUTTON) AndAlso (state = GLUT_UP) Then dark=0 
  
End Sub


Sub mouseMoving Cdecl (ByVal x As Integer ,ByVal y As Integer)

	if(x<580) AndAlso (GG.addSect=0) AndAlso (GG.move(0)>-1) Then 
	
		Dim As Integer Aw=GG.move(0), Ax=GG.move(1) 
		Dim As Integer Bw=GG.move(2), Bx=GG.move(3) 
		if(Ax=1) Then WW(Aw).x1=((x+16) Shr 5) Shl 5: WW(Aw).y1=((GLSH-y+16) Shr 5) Shl 5 
		if(Ax=2) Then WW(Aw).x2=((x+16) Shr 5) Shl 5: WW(Aw).y2=((GLSH-y+16) Shr 5) Shl 5 
		if(Bx=1) Then WW(Bw).x1=((x+16) Shr 5) Shl 5: WW(Bw).y1=((GLSH-y+16) Shr 5) Shl 5 
		if(Bx=2) Then WW(Bw).x2=((x+16) Shr 5) Shl 5: WW(Bw).y2=((GLSH-y+16) Shr 5) Shl 5 
		
	EndIf
  
End Sub

Sub KeysDown Cdecl (ByVal key As Integer , ByVal x As Integer , ByVal y As Integer)
	key=key And 255
	If key=Asc("w") Then KK.w =1 
	If key=Asc("s") Then KK.s =1 
	If key=Asc("a") Then KK.a =1 
	If key=Asc("d") Then KK.d =1 
	If key=Asc("m") Then KK.m =1 
	If key=Asc(",") Then KK.sr=1 
	If key=Asc(".") Then KK.sl=1 
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

Sub movePlayer()

	'move up, down, left, right
	if(KK.a =1 AndAlso KK.m=0) Then 
		PP.a-=4
		if(PP.a<0) Then PP.a+=360 
	EndIf
	
	if(KK.d =1) AndAlso (KK.m=0) Then 
		PP.a+=4
		if(PP.a>359) Then PP.a-=360 
	EndIf
	
	Dim As Integer dx=MM.sin_t(PP.a)*10.0 
	Dim As Integer dy=MM.cos_t(PP.a)*10.0 
	if(KK.w =1) AndAlso (KK.m=0) Then PP.x+=dx: PP.y+=dy 
	if(KK.s =1) AndAlso (KK.m=0) Then PP.x-=dx: PP.y-=dy 
	
	
	'strafe left, right
	if(KK.sr=1) Then PP.x+=dy: PP.y-=dx 
	if(KK.sl=1) Then PP.x-=dy: PP.y+=dx 
	
	'move up, down, look up, look down
	if(KK.a=1) AndAlso (KK.m=1) Then PP.l-=1 
	if(KK.d=1) AndAlso (KK.m=1) Then PP.l+=1 
	if(KK.w=1) AndAlso (KK.m=1) Then PP.z-=4 
	if(KK.s=1) AndAlso (KK.m=1) Then PP.z+=4 

End Sub

Sub display Cdecl ()
	 Dim As Integer x,y 
	 if(TT.fr1-TT.fr2>=50) Then 'only draw 20 frames/second
	  
		  movePlayer() 
		  draw2D() 
		  darken() 
		
		  TT.fr2=TT.fr1 
		  glutSwapBuffers() 
		  glutReshapeWindow(GLSW,GLSH)              'prevent window scaling
	 
	 EndIf
	
	 TT.fr1=glutGet(GLUT_ELAPSED_TIME)           '1000 Milliseconds per second
	 glutPostRedisplay() 
End Sub

Function shade(w As Integer) As Integer    'automatic shading

	Dim As Single ang = ATan2(WW(w).y2-WW(w).y1,WW(w).x2-WW(w).x1) 
	ang=(ang*180)/M_PI      'radians to degrees
	If (ang<0) Then ang+=360 'correct negative
	Dim As Integer shades=ang            'shading goes from 0-90-0-90-0
	If (shades>180) Then shades=180-(shades-180) 
	If (shades> 90) Then shades= 90-(shades- 90) 

	return shades*0.75 
End Function

Sub init()
	 Dim As Integer x 
	 initGlobals() 
	
	 'init player
	 PP.x=32*9: PP.y=48: PP.z=30: PP.a=0: PP.l=0     'init player variables

	 'store sin_t/cos_t in degrees
	 for x=0 To 360-1 'precalulate sin_t cos_t in degrees
	  	MM.cos_t(x)=Cos(x/180.0*M_PI) 
	  	MM.sin_t(x)=sin(x/180.0*M_PI) 
	 Next

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

	 'for alpha overlay darken buttons
	 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA) 
	 glEnable(GL_BLEND) 
End Sub



 'glutInit(@argc, argv) 
 glutInitDisplayMode(GLUT_DOUBLE Or GLUT_RGB) 
 glutInitWindowPosition(GLSW/2,GLSH/2) 
 glutInitWindowSize(GLSW,GLSH) 
 glutCreateWindow("Grid2D v1.0 by:3DSage") 
 glPointSize(pixelScale)                         'pixel size
 gluOrtho2D(0,GLSW,0,GLSH)                       'origin bottom left
 init() 
 glutDisplayFunc(@display) 
 glutKeyboardFunc(@KeysDown) 
 glutKeyboardUpFunc(@KeysUp) 
 glutMouseFunc(@mouse) 
 glutMotionFunc(@mouseMoving) 
 glutMainLoop() 

