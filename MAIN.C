#include "REFLECT.h"
#include <utility.h>
#include <rs232.h>
#include <analysis.h>
#include <formatio.h>
#include <ansi_c.h>
#include <cvirte.h>		/* Needed if linking in external compiler; harmless otherwise */
#include <userint.h>

#include "reflect.h"
#include "easytab.h"    

#ifndef FALSE
#define FALSE 	0
#define TRUE 	!FALSE
#endif


#define MAXLEN 4096
#define SWIDTH (Wave.len/5)


//*****************************************************

struct _tagDefault
{
	union
	{
		char p[500];
		struct _tagSet
		{
			int 	splen;
			int		spfreq;
			int		spnum;
			int		sptaglen;
			int		sptagvol;
			int		spwtime;
			int		spcom;
			int		spcgq;
			
			int 	fttype;
			int 	ftorder;
			double	ftfcl;
			double	ftfch;
			
			double	fxrate;
			double	fxlength;
			double	fxvunit; 
			double	fxaunit;                       
		} Item;
	}buf;
};


struct _tagBottom
{
  	double pos;
   	int type;
};
    		
struct _tagFault
{
   	double	pos;
   	int		type;
   	int		level;
};

struct _tagPOLE
{
    double 				top;
	int					faultnum;      
	struct _tagBottom  	bottom;
    struct _tagFault  	fault[20];
    
};

struct _tagSampling
{
    int 	splen;
    int		spfreq;
    int		tglen;
    int		tgvol;
    int		wcfreq;
    int 	cgq;
    double	vunit;//灵敏度
    double	aunit;//灵敏度                         
};


struct _tagPrint
{
	char	date[16];
	char 	no[32];
	char	place[128];
};

struct _tagWAVE
{
	int		len;
 	int 	wvPenColor;
 	int 	zoom;    
    double  Y[MAXLEN]; 	//原始值    
    double  Yt[MAXLEN];	//时域值
    double  Yf[MAXLEN];	//频域值
};

struct _tagSave
{
	union
	{
		char p[100000];
		struct _tagSavePa
		{
			int 				mode;
			double				rate;
			double				length;
			struct _tagSampling	Sampling;
			struct _tagPOLE		Pole;
			struct _tagWAVE		Wave;
			struct _tagPrint	Print;
		} Item;
	} buf;
};

//******************************************************************

static int 	phMain,phSetting,phSampling,phFault,phFilter,phAnalyse,phPrint;
static int 	pWave,pPole;
static int 	settab;
static int 	mode =0;
int     	hPopmenu,i;			 

struct _tagPOLE		Pole;
struct _tagSampling	Sampling;
struct _tagWAVE		Wave;
struct _tagPrint    Print;
struct _tagDefault  Default;

int 	PrintReport(int pmode); //0-Current;1-File;
int 	InitZT(void);
int 	DrawZT(void);
int 	DrawWT(double range);
int 	DrawWF(double range);
void 	UpdataWT(void);
void 	UpdataWF(void);
int  	GetDefault(void);
///********************************************************************************///
static int ClickIsOnCtrl (int panel, int control, int x, int y)
{
    int height, width;
    if ((x < 0) || (y < 0))  			return FALSE;
    if (control == 0) 
    {
        GetPanelAttribute (panel, ATTR_HEIGHT, &height);
        GetPanelAttribute (panel, ATTR_WIDTH, &width);
    }
    else 
    {
        GetCtrlAttribute (panel, control, ATTR_HEIGHT, &height);
        GetCtrlAttribute (panel, control, ATTR_WIDTH, &width);
    }
    if ((y < height) && (x < width))	return TRUE;
    else								return FALSE;
}

static void popMaxFocus (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	double 	buf[1024];
	double 	dx,dy,maxVal,minVal;
	int 	maxIndex,minIndex;
	int		i,ix;
	
	GetGraphCursor (panel, MAINPANEL_TSCOPE, 1,&dx ,&dy );
	ix=dx*Sampling.spfreq/1000.0-SWIDTH/2;			ix=(ix<0)?0:ix;
	for(i=0;i<SWIDTH;i++) 	buf[i] = Wave.Yt[ix+i];
	MaxMin1D (buf, SWIDTH, 	&maxVal, &maxIndex, &minVal, &minIndex);
	SetGraphCursor (panel, MAINPANEL_TSCOPE, 1,1000.0*(ix+maxIndex)/Sampling.spfreq,Wave.Yt[ix+maxIndex]);
	UpdataWT();
}

static void popMinFocus (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	double 	dx,dy;
	double 	buf[1024];
	double 	maxVal,minVal;
	int 	maxIndex,minIndex;
	int		i,ix;
	
	GetGraphCursor (panel, 	MAINPANEL_TSCOPE, 1,&dx ,&dy );
	ix=dx*Sampling.spfreq/1000.0-SWIDTH/2;			ix=(ix<0)?0:ix;
	for(i=0;i<SWIDTH;i++) 	buf[i] = Wave.Yt[ix+i];
	MaxMin1D (buf, SWIDTH, 	&maxVal, &maxIndex, &minVal, &minIndex);
	SetGraphCursor (panel, MAINPANEL_TSCOPE, 1,1000.0*(ix+minIndex)/Sampling.spfreq ,Wave.Yt[ix+minIndex] );
	UpdataWT();
}

static void popFlaut (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	double 	dx,dy;
	
	GetGraphCursor (panel, 	MAINPANEL_TSCOPE, 1,&dx ,&dy );
	Pole.faultnum++;
	Pole.fault[Pole.faultnum].type 	= 0;
	Pole.fault[Pole.faultnum].level = 0;
	Pole.fault[Pole.faultnum].pos	= dx;
	DrawZT();
}


static void popFlautHead(int menuBar, int menuItem, void *callbackData,
		int panel)
{
	double 	dx,dy;
	GetGraphCursor (panel, MAINPANEL_TSCOPE, 1,&dx ,&dy );
	Pole.top =dx;
	DrawZT();
}

static void popFlautBottom(int menuBar, int menuItem, void *callbackData,
		int panel)
{
	double 	dx,dy;
	GetGraphCursor (phMain, MAINPANEL_TSCOPE, 1,&dx ,&dy );
	Pole.bottom.pos 	= dx;
	if(Pole.bottom.type>2)  Pole.bottom.type= 0;
	DrawZT();
}

///********************************************************************************///
//main program	& Callback Route
int Initialize(void)
{
	double	X[MAXLEN],Y[MAXLEN];
	double	mag[MAXLEN],phase[MAXLEN];
	
	Wave.len = 1024;
	Wave.zoom=1;
    Sampling.spfreq	=25000;
    Wave.wvPenColor = 0xffffff;		
    GetDefault();
    Sampling.cgq =Default.buf.Item.spcgq;
	for(i=0;i<Wave.len;i++)
	{
		Wave.Y[i]	=-10*(sin(0.02*i)+sin(0.1*i)*0.2) * exp(-0.005*i);
		Wave.Yt[i]	=Wave.Y[i];
	}			   
	Copy1D(Wave.Yt,Wave.len,X);	
	Set1D (Y,Wave.len, 0);
	FFT (X, Y, Wave.len);
	ToPolar1D(X,Y,Wave.len,mag,phase);
	for(i=0;i<Wave.len;i++)		Wave.Yf[i]=mag[i]/Wave.len;
	
    Pole.faultnum=0;
    Pole.top	=0;
    Pole.bottom.pos	= Wave.len;
    return 0;
}

int GetFaultType(int i,char *type)
{
	
	switch(i)
		{
			case 0:	sprintf(type, "离析");break;
			case 1:	sprintf(type, "断裂");break;
			case 2:	sprintf(type, "加泥");break;
			case 3:	sprintf(type, "缩颈");break;
			case 4: sprintf(type, "扩颈");break;
			default:sprintf(type, "离析");break;
		}			  
	return 0;
}
		
int GetFaultLevel(int i,char *level)
{
	
	switch(i)
		{
			case 0: sprintf(level, "轻微");break;
			case 1: sprintf(level, "明显");break;
			case 2: sprintf(level, "严重");break;
			default:sprintf(level, "轻微");break;
		}
	return 0;
}

int DrawWT(double range)
{
	double 	max,min;
	int 	imax,imin;
	char 	msg[100];
	double 	x[4096];

	//Wave
	DeleteGraphPlot (phMain, MAINPANEL_TSCOPE, -1, VAL_IMMEDIATE_DRAW);
	for(i=0;i<Wave.len;i++)	x[i]=1000.0*i/Sampling.spfreq;
	PlotXY (phMain, MAINPANEL_TSCOPE, x, Wave.Yt, Wave.len, VAL_DOUBLE,
			 VAL_DOUBLE, VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1,
			 VAL_WHITE);
	MaxMin1D (Wave.Yt, Wave.len, &max, &imax, &min, &imin);
	max= ((max<-min)?-min:max);
	SetAxisRange (	phMain, MAINPANEL_TSCOPE, 
					VAL_MANUAL,	0.0, 	x[Wave.len-1]*range,
					VAL_MANUAL,	-max, 	max);
	return 0;
}

int InitZT(void)
{
	Pole.top		=0;
	Pole.bottom.pos	=0;
	Pole.bottom.type=0;
	Pole.faultnum	=0;
	DeleteGraphPlot (phMain, MAINPANEL_ZSCOPE, -1, VAL_IMMEDIATE_DRAW);
	return 0;
}

int DrawZT()
{   
	float		ZTWidth	=40;
	float		ZTBotY	=60;
	float		ZTBotX	=1;
	float		ZTFaultX=1; 
	
	char 	msg[100],type[100],level[100];
	int 	imax,imin;
	int 	i;
	double 	max,min;
	double  len0,v0;
	double 	X[3],Y[3];
	double 	x[4096];
	double  t;
	double  Ts=1000.0/Sampling.spfreq;
	
	if(Pole.faultnum==0 && Pole.top==0) DeleteGraphPlot (phMain, MAINPANEL_ZSCOPE, -1, VAL_IMMEDIATE_DRAW);
	if(mode==0)	return 1;
	ClearListCtrl (phMain, MAINPANEL_LISTBOX);
	//边界设定
	SetAxisRange (	phMain, MAINPANEL_ZSCOPE, 
					VAL_MANUAL,	0.0, 	Ts*(Wave.len-1),
					VAL_MANUAL,	-100, 	100);
	ZTBotX	=Ts*(Wave.len-1)/50.0;
	ZTFaultX=Ts*(Wave.len-1)/200.0;
	
	//桩体外形
	DeleteGraphPlot (phMain, MAINPANEL_ZSCOPE, -1, VAL_IMMEDIATE_DRAW);
	PlotRectangle (	phMain, MAINPANEL_ZSCOPE, 
					Pole.top, 				-ZTWidth,
					Pole.bottom.pos,		ZTWidth, 
					VAL_LT_GRAY,VAL_DK_MAGENTA);//VAL_GRAY, VAL_GRAY);
	//桩长
	GetCtrlVal (phMain, MAINPANEL_RATE,		&v0);
	GetCtrlVal (phMain, MAINPANEL_LENGTH,	&len0);
	
	t=(Pole.bottom.pos - Pole.top)/1000.0;					 
	sprintf(msg, "估计桩长:%4.2f",	v0*t*0.5);
	SetCtrlVal (phMain, MAINPANEL_ZCMSG,msg);
	sprintf(msg, "估计波速:%4.2f",	len0*2.0/t);
	SetCtrlVal (phMain, MAINPANEL_BSMSG,msg);
	//指数放大
	sprintf(msg, "指数放大倍数:  %i",
					Wave.zoom);
	InsertListItem (phMain, MAINPANEL_LISTBOX, 0, msg, 0);

	//桩底
	switch(Pole.bottom.type)
	{
		case 0: sprintf(msg, "桩底类型： 平底");
				break;
		case 1: sprintf(msg, "桩底类型： 尖底");
				X[0] = Pole.bottom.pos;			Y[0] = ZTWidth;
				X[1] = Pole.bottom.pos;			Y[1] = -ZTWidth;
				X[2] = Pole.bottom.pos+ZTBotX;	Y[2] = 0;
				PlotPolygon (phMain, MAINPANEL_ZSCOPE,X,Y,3,VAL_DOUBLE,VAL_DOUBLE,VAL_LT_GRAY,VAL_DK_MAGENTA);
				break;
		case 2:	sprintf(msg, "桩底类型： 扩底");
				X[0] = Pole.bottom.pos;			Y[0] = ZTBotY;
				X[1] = Pole.bottom.pos+ZTBotX;	Y[1] = -ZTBotY;
				PlotRectangle (phMain, MAINPANEL_ZSCOPE,X[0],Y[0],X[1],Y[1],VAL_LT_GRAY,VAL_DK_MAGENTA);
				break;
		default:sprintf(msg, "桩底类型： 平底");
				break;
				
	}
	
	InsertListItem (phMain, MAINPANEL_LISTBOX, 1, msg, 1);
	//桩体缺陷
	for(i=1;i<=Pole.faultnum;i++)
	{   
		GetFaultLevel(Pole.fault[i].level,	level);
		GetFaultType(Pole.fault[i].type,	type);
		sprintf(msg, "%i",	i);
		PlotText (	phMain,MAINPANEL_ZSCOPE , Pole.fault[i].pos+0.1, -35, msg, 
					VAL_APP_META_FONT, VAL_WHITE,VAL_TRANSPARENT);
		X[0] = Pole.fault[i].pos-ZTFaultX;		Y[0] = -ZTWidth;
		X[1] = Pole.fault[i].pos+ZTFaultX;		Y[1] = ZTWidth;
		PlotRectangle (phMain, MAINPANEL_ZSCOPE, X[0], Y[0], X[1], Y[1],
					   VAL_LT_GRAY, VAL_LT_GRAY);
		t=(Pole.fault[i].pos - Pole.top)/1000.0;
		sprintf(msg, "%i:%s,%s,距离桩头%5.2f米",	i,type,level,v0*t*0.5);
		InsertListItem (phMain, MAINPANEL_LISTBOX, i+1, msg, i+1);
	}
	return 0;
}

int DrawWF(double range)
{
	double  min,max;
	int		i,imin,imax;
	
	double	X[MAXLEN],Y[MAXLEN];
	double	mag[MAXLEN],phase[MAXLEN];
	double	x[MAXLEN];
	
	//calculate FFT
	Copy1D(Wave.Yt,Wave.len,X);	  
	Set1D (Y,Wave.len, 0);
	FFT (X, Y, Wave.len);
	ToPolar1D(X,Y,Wave.len,mag,phase);
	for(i=0;i<Wave.len;i++)		Wave.Yf[i]=mag[i]/Wave.len;
	
	DeleteGraphPlot (phMain, MAINPANEL_FSCOPE, -1, VAL_IMMEDIATE_DRAW);
	for(i=0;i<Wave.len;i++)	x[i]=1.0*i*Sampling.spfreq/Wave.len;
	PlotXY (phMain, MAINPANEL_FSCOPE, x, Wave.Yf, Wave.len, VAL_DOUBLE,
			 VAL_DOUBLE, VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1,
			 VAL_WHITE);
	
	MaxMin1D (Wave.Yf, Wave.len, &max, &imax, &min, &imin);
	SetAxisRange (	phMain, MAINPANEL_FSCOPE, 
					VAL_MANUAL,	0.0, 	x[Wave.len-1]*range,
					VAL_MANUAL,	0, 	max);
	return 0;
}
				  	
				  	
				  	
void UpdataWT(void)
{//Time Wave			
	double i;
	//GetCtrlVal (phMain, MAINPANEL_TSCALE, 	&i);
	DrawWT(1);		
}	


void UpdataWF(void)
{//UpData Freq Wave
	//double i;
	//GetCtrlVal (phMain, MAINPANEL_FSCALE, 	&i);
	DrawWF(0.5);
}
			
			
//***********main******************
int main (int argc, char *argv[])
{
	if (InitCVIRTE (0, argv, 0) == 0)	/* Needed if linking in external compiler; harmless otherwise */
		return -1;	/* out of memory */
	if ((phMain = LoadPanel (0, "reflect.uir", MAINPANEL)) < 0)
		return -1;
	
	Initialize();
	DrawWT(1);
	DrawWF(0.5);
	GetDefault();
	SetCtrlVal(phMain,MAINPANEL_RATE,	Default.buf.Item.fxrate);
	SetCtrlVal(phMain,MAINPANEL_LENGTH,	Default.buf.Item.fxlength);
	DisplayPanel (phMain);
	RunUserInterface ();
	return 0;
}
//***********main******************


int CVICALLBACK TScopeCallback (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int 	popmenubar,popmenu;
	int 	X,Y,index;
	char	msg[400];
	double 	dx,dy,dx1,dy1;
	double  px,py;
	
	double 	buf[1024];
	double 	maxVal,minVal;
	int 	maxIndex,minIndex;
	int		i,ix;
	
	
	switch (event)
		{
		case EVENT_RIGHT_CLICK:  
			if(mode)
			{
		        GetRelativeMouseState (panel, 0, &X, &Y, NULL,NULL,NULL);
				popmenubar  = NewMenuBar (0);
			    popmenu   = NewMenu (popmenubar, "Abacus", -1);      
			    NewMenuItem (popmenubar, popmenu, "波峰聚焦",		-1, 0, popMaxFocus, 0);
			   	NewMenuItem (popmenubar, popmenu, "波谷聚焦",		-1, 0, popMinFocus, 0);
				InsertSeparator (popmenubar,popmenu ,-1 );
			    NewMenuItem (popmenubar, popmenu, "桩头定位",			-1, 0, popFlautHead,0);  
			    NewMenuItem (popmenubar, popmenu, "桩尾定位",			-1, 0, popFlautBottom, 0);  
			    NewMenuItem (popmenubar, popmenu, "缺陷添加",			-1, 0, popFlaut, 	0);
			   	RunPopupMenu (popmenubar, popmenu, panel, Y,X, 0, 0, 0, 0);
				DiscardMenuBar (popmenubar);
			}
			break;
		case EVENT_VAL_CHANGED :
			GetGraphCursor (phMain, MAINPANEL_TSCOPE, 1,&dx ,&dy );
			sprintf(msg, "时间:%4.2fms 速度:%6.2fcm/s", 
					dx,Wave.Yt[(int)(dx*Sampling.spfreq/1000.0)]);
					
    		SetCtrlVal(phMain, MAINPANEL_TIMEMSG, msg);
			break;
		}
	return 0;
}

int CVICALLBACK Quit1 (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			QuitUserInterface (0);
			break;
		}
	return 0;
}

//********************************************************
//设置面板
//********************************************************

void CVICALLBACK miSetting (int menuBar, int menuItem, void *callbackData,
		int panel)
{
		phSetting = LoadPanel(phMain, "reflect.uir",DFTPANEL);
		SetCtrlVal(phSetting,DFTPANEL_SPLEN,	Default.buf.Item.splen);
		SetCtrlVal(phSetting,DFTPANEL_SPFREQ,	Default.buf.Item.spfreq);
		SetCtrlVal(phSetting,DFTPANEL_SPNUM,	Default.buf.Item.spnum);
		SetCtrlVal(phSetting,DFTPANEL_SPTAGLEN,	Default.buf.Item.sptaglen);
		SetCtrlVal(phSetting,DFTPANEL_SPTAGVOL,	Default.buf.Item.sptagvol);
		SetCtrlVal(phSetting,DFTPANEL_SPWTIME,	Default.buf.Item.spwtime);
		SetCtrlVal(phSetting,DFTPANEL_SPCOM,	Default.buf.Item.spcom);
		SetCtrlVal(phSetting,DFTPANEL_SPCGQ,	Default.buf.Item.spcgq);
	
		SetCtrlVal(phSetting,DFTPANEL_FTTYPE,	Default.buf.Item.fttype);
		SetCtrlVal(phSetting,DFTPANEL_FTORDER,	Default.buf.Item.ftorder);
		SetCtrlVal(phSetting,DFTPANEL_FTFCL,	Default.buf.Item.ftfcl);
		SetCtrlVal(phSetting,DFTPANEL_FTFCH,	Default.buf.Item.ftfch);
	
		SetCtrlVal(phSetting,DFTPANEL_FXRATE,	Default.buf.Item.fxrate);
		SetCtrlVal(phSetting,DFTPANEL_FXLENGTH,	Default.buf.Item.fxlength);
		SetCtrlVal(phSetting,DFTPANEL_FXVUNIT,	Default.buf.Item.fxvunit);
		SetCtrlVal(phSetting,DFTPANEL_FXAUNIT,	Default.buf.Item.fxaunit);
	
		InstallPopup (phSetting);
		
}

int CVICALLBACK SetQuit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DiscardPanel(GetActivePanel ());
			break;
		}
	return 0;
}

int CVICALLBACK SetOK (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	char FileName[300],DirPath[500];
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal(phSetting,DFTPANEL_SPLEN,	&Default.buf.Item.splen);
			GetCtrlVal(phSetting,DFTPANEL_SPFREQ,	&Default.buf.Item.spfreq);
			GetCtrlVal(phSetting,DFTPANEL_SPNUM,	&Default.buf.Item.spnum);
			GetCtrlVal(phSetting,DFTPANEL_SPTAGLEN,	&Default.buf.Item.sptaglen);
			GetCtrlVal(phSetting,DFTPANEL_SPTAGVOL,	&Default.buf.Item.sptagvol);
			GetCtrlVal(phSetting,DFTPANEL_SPWTIME,	&Default.buf.Item.spwtime);
			GetCtrlVal(phSetting,DFTPANEL_SPCOM,	&Default.buf.Item.spcom);
			GetCtrlVal(phSetting,DFTPANEL_SPCGQ,	&Default.buf.Item.spcgq);
			
			GetCtrlVal(phSetting,DFTPANEL_FTTYPE,	&Default.buf.Item.fttype);
			GetCtrlVal(phSetting,DFTPANEL_FTORDER,	&Default.buf.Item.ftorder);
			GetCtrlVal(phSetting,DFTPANEL_FTFCL,	&Default.buf.Item.ftfcl);
			GetCtrlVal(phSetting,DFTPANEL_FTFCH,	&Default.buf.Item.ftfch);
			
			GetCtrlVal(phSetting,DFTPANEL_FXRATE,	&Default.buf.Item.fxrate);
			GetCtrlVal(phSetting,DFTPANEL_FXLENGTH,	&Default.buf.Item.fxlength);
			GetCtrlVal(phSetting,DFTPANEL_FXVUNIT,	&Default.buf.Item.fxvunit);
			GetCtrlVal(phSetting,DFTPANEL_FXAUNIT,	&Default.buf.Item.fxaunit);
			
			GetDir (DirPath);
			sprintf(FileName,"%s\\default.opt",DirPath);
			ArrayToFile (FileName, Default.buf.p, VAL_CHAR, 500, 1,
				 VAL_GROUPS_TOGETHER, VAL_GROUPS_AS_COLUMNS,
				 VAL_CONST_WIDTH, 1, VAL_BINARY, VAL_TRUNCATE);  
			DiscardPanel(phSetting);
			SetCtrlVal(phMain,MAINPANEL_LENGTH,	Default.buf.Item.fxlength);
			SetCtrlVal(phMain,MAINPANEL_RATE,	Default.buf.Item.fxrate);
			break;
		}
	return 0;
}

int  GetDefault(void)
{	
	char FileName[300];
	GetDir (FileName);
	sprintf(FileName,"%s\\default.opt",FileName);
	FileToArray (FileName, Default.buf.p, VAL_UNSIGNED_CHAR, 500, 1,
				  VAL_GROUPS_TOGETHER, VAL_GROUPS_AS_COLUMNS, VAL_BINARY);
	return 0;
}
//******************************************************************//
//							菜单项命令开始							//
//******************************************************************//

int Open(char *file)
{
	int 				i;
	struct _tagSave		Save;
	
	i=FileToArray (file, Save.buf.p, VAL_UNSIGNED_CHAR, 100000, 1,
				  VAL_GROUPS_TOGETHER, VAL_GROUPS_AS_COLUMNS, VAL_BINARY);
	
	//界面参数
	SetCtrlVal (phMain, MAINPANEL_RATE,			Save.buf.Item.rate);
	SetCtrlVal (phMain, MAINPANEL_LENGTH,		Save.buf.Item.length);
	SetCtrlVal (phMain, MAINPANEL_BINARYSWITCH,	Save.buf.Item.mode);
	mode 	=Save.buf.Item.mode; 
	
	//采样参数
	Sampling.splen			=	Save.buf.Item.Sampling.splen; 
	if(Sampling.splen>4096) Sampling.splen=4096;
	Sampling.spfreq			=	Save.buf.Item.Sampling.spfreq;
	Sampling.tglen			=	Save.buf.Item.Sampling.tglen;
	Sampling.tgvol			=	Save.buf.Item.Sampling.tgvol;
	Sampling.wcfreq			=	Save.buf.Item.Sampling.wcfreq;
	Sampling.aunit			=	Save.buf.Item.Sampling.aunit;
	Sampling.vunit			=	Save.buf.Item.Sampling.vunit;
	Sampling.cgq			=	Save.buf.Item.Sampling.cgq;

	//Print parameter	
	CopyBytes (Print.no,   0, Save.buf.Item.Print.no, 		0, sizeof(Print.no));
	CopyBytes (Print.date, 0, Save.buf.Item.Print.date, 	0, sizeof(Print.date));
	CopyBytes (Print.place,0, Save.buf.Item.Print.place,	0, sizeof(Print.place));

	//波形参数	
	Wave.zoom				=Save.buf.Item.Wave.zoom;
	Wave.len				=	Save.buf.Item.Wave.len;
	if(Wave.len>4096) 		Wave.len=4096;
	Wave.wvPenColor			=	Save.buf.Item.Wave.wvPenColor;
	for(i=0;i<MAXLEN;i++)
	{
		Wave.Y[i]			=	Save.buf.Item.Wave.Y[i];
		Wave.Yt[i]			=	Save.buf.Item.Wave.Yt[i];
		Wave.Yf[i]			=	Save.buf.Item.Wave.Yf[i];
	}		
	//桩体参数
	Pole.top			=	Save.buf.Item.Pole.top;
	Pole.bottom.pos		=	Save.buf.Item.Pole.bottom.pos;
	Pole.bottom.type	=	Save.buf.Item.Pole.bottom.type;
	Pole.faultnum		=	Save.buf.Item.Pole.faultnum;
	for(i=0;i<20;i++)
	{
		Pole.fault[i].pos		=	Save.buf.Item.Pole.fault[i].pos;
		Pole.fault[i].type		= 	Save.buf.Item.Pole.fault[i].type;
		Pole.fault[i].level		=	Save.buf.Item.Pole.fault[i].level;
	}
	return 0;
}

int Save(char *file)
{
	int i;
	struct _tagSave		Save;
	//界面参数
	GetCtrlVal (phMain, MAINPANEL_RATE,		&Save.buf.Item.rate);
	GetCtrlVal (phMain, MAINPANEL_LENGTH,	&Save.buf.Item.length);
	GetCtrlVal (phMain, MAINPANEL_BINARYSWITCH,	&Save.buf.Item.mode);
	//采样参数
	Save.buf.Item.Sampling.splen	=Sampling.splen;
	Save.buf.Item.Sampling.spfreq	=Sampling.spfreq;
	Save.buf.Item.Sampling.tglen	=Sampling.tglen;
	Save.buf.Item.Sampling.tgvol	=Sampling.tgvol;
	Save.buf.Item.Sampling.wcfreq	=Sampling.wcfreq;
	Save.buf.Item.Sampling.aunit	=Sampling.aunit;
	Save.buf.Item.Sampling.vunit	=Sampling.vunit;
	Save.buf.Item.Sampling.cgq		=Sampling.cgq;
	
	//Print parameter				
	CopyBytes (Save.buf.Item.Print.no,   0, Print.no, 		0, sizeof(Print.no));
	CopyBytes (Save.buf.Item.Print.date, 0, Print.date, 	0, sizeof(Print.date));
	CopyBytes (Save.buf.Item.Print.place,0, Print.place,	0, sizeof(Print.place));

	//波形参数
	Save.buf.Item.Wave.zoom			=Wave.zoom;
	Save.buf.Item.Wave.len			=Wave.len;
	Save.buf.Item.Wave.wvPenColor	=Wave.wvPenColor;
	for(i=0;i<MAXLEN;i++)
	{
		Save.buf.Item.Wave.Y[i]		=Wave.Y[i];
		Save.buf.Item.Wave.Yt[i]	=Wave.Yt[i];
		Save.buf.Item.Wave.Yf[i]	=Wave.Yf[i];
	}
	//桩体参数
	Save.buf.Item.Pole.top			=Pole.top;
	Save.buf.Item.Pole.bottom.pos   =Pole.bottom.pos;
	Save.buf.Item.Pole.bottom.type  =Pole.bottom.type;
	Save.buf.Item.Pole.faultnum		=Pole.faultnum;
	for(i=0;i<20;i++)
	{
		Save.buf.Item.Pole.fault[i].pos		=Pole.fault[i].pos;
		Save.buf.Item.Pole.fault[i].type	=Pole.fault[i].type;
		Save.buf.Item.Pole.fault[i].level   =Pole.fault[i].level;
	}
	
	ArrayToFile (file, Save.buf.p, VAL_CHAR, 100000, 1,
				 VAL_GROUPS_TOGETHER, VAL_GROUPS_AS_COLUMNS,
				 VAL_CONST_WIDTH, 1, VAL_BINARY, VAL_TRUNCATE);
	return 0;
}


void CVICALLBACK miOpenfile (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int i;
	char FileName[300],DirPath[500];

//	GetDir (DirPath);
//	sprintf(DirPath,"%s\\data\\*.dat",DirPath);					         
	sprintf(FileName,"%s","*.dat");
	//if(0==FileSelectPopup(DirPath,FileName,"*.dat","打开文件",
	//					VAL_OK_BUTTON,0,1,1,0,FileName))
	//	return;
	if(0==FileSelectPopup ("", FileName, "*.dat", "打开文件", VAL_OK_BUTTON, 0,
						   1, 1, 0, FileName))
		return;
	SetCtrlVal(phMain,MAINPANEL_OPENFILE,FileName);
	Open(FileName);
	DrawWT(1);
	DrawWF(0.5);
	DrawZT();
}

//文件存储
int phSave;
void CVICALLBACK miSavefile (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int Day,Month,Year;
	phSave = LoadPanel(phMain, "reflect.uir",SAVEPAHEL);
	GetSystemDate (&Month, &Day, &Year);
	sprintf(Print.date,"%4i-%i-%i",Year,Month,Day);
	SetCtrlVal(phSave,SAVEPAHEL_NUMBER,	Print.no);
	SetCtrlVal(phSave,SAVEPAHEL_DATE,	Print.date);
	SetCtrlVal(phSave,SAVEPAHEL_PLACE,	Print.place);
	InstallPopup (phSave);
}

int CVICALLBACK SaveOK (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int i;
	char FileName[300],DirPath[500];
	
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal(phSave,SAVEPAHEL_NUMBER,	Print.no);
			GetCtrlVal(phSave,SAVEPAHEL_DATE,	Print.date);
			GetCtrlVal(phSave,SAVEPAHEL_PLACE,	Print.place);
			DiscardPanel(phSave);

			
			GetDir (DirPath);
			if(0!=strcmp (Print.no,""))		sprintf(FileName, "%s.dat",Print.no);
			else							sprintf(FileName, "%s","*.dat");
			sprintf(DirPath,"%s\\data\\*.dat",DirPath);					         
			if(0==FileSelectPopup (DirPath, FileName, "*.dat", "保存文件",
								   VAL_SAVE_BUTTON, 0, 1, 1, 0, FileName))
								return 0;
			SetCtrlVal(phMain,MAINPANEL_OPENFILE,FileName);
			Save(FileName);
			break;
		}
	return 0;
}

int CVICALLBACK SaveQuit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DiscardPanel(GetActivePanel ());
			break;
		}
	return 0;
}



void CVICALLBACK Quit (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	QuitUserInterface (0);
}

//******************************************************************//
//						采样面板
//******************************************************************//
int com=1;          
char scom[8];

int CVICALLBACK ComTest (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int i,com;
	unsigned char s_buf[10],r_buf[10];

	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal(phSampling,SPPANEL_COM,	&com);
			sprintf(scom,"COM%i",com);
			OpenComConfig (com, scom, 57600, 0, 8, 1, 512, 512);
			SetComTime (com, 0.1);
			FlushInQ (com);
			s_buf[0] 	= 0x00;
			s_buf[1]	= 0xaa;
			ComWrt 	(com, s_buf ,2);
			ComRd 	(com, r_buf, 1);
			if(r_buf[0] == 0x55)   MessagePopup ("提示信息", "通讯正常！");
			else 				   MessagePopup ("提示信息", "通讯失败！");	
			CloseCom (com);
			break;
		}
	return 0;
}

	

void CVICALLBACK miSampling (int menuBar, int menuItem, void *callbackData,
		int panel)
{   
	int num0;
	SetCtrlVal(phMain,MAINPANEL_OPENFILE,"采样结果未保存。");	
	
	phSampling = LoadPanel (phMain, "reflect.uir", SPPANEL);
	GetDefault();
	SetCtrlVal(phSampling,SPPANEL_SPLEN,	Default.buf.Item.splen);
	SetCtrlVal(phSampling,SPPANEL_SPFREQ,	Default.buf.Item.spfreq);
	SetCtrlVal(phSampling,SPPANEL_AVNUM,	Default.buf.Item.spnum);
	SetCtrlVal(phSampling,SPPANEL_TGLEN,	Default.buf.Item.sptaglen);
	
	SetCtrlVal(phSampling,SPPANEL_TGVOL,	Default.buf.Item.sptagvol);
	SetCtrlVal(phSampling,SPPANEL_CGQ,		Default.buf.Item.spcgq);
	SetCtrlVal(phSampling,SPPANEL_VUNIT,		Default.buf.Item.fxvunit);
	SetCtrlVal(phSampling,SPPANEL_AUNIT,		Default.buf.Item.fxaunit);
	
	SetCtrlVal(phSampling,SPPANEL_WAITNUMERIC,	Default.buf.Item.spwtime);
	SetCtrlVal(phSampling,SPPANEL_COM,		Default.buf.Item.spcom);
	InstallPopup (phSampling);
}

int CVICALLBACK ChangCom (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int num0;
	
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal(phSampling,SPPANEL_COM,	&com);
			break;
		}
	return 0;
}

int CVICALLBACK SPQuit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DiscardPanel(phSampling);
			break;
		}
	return 0;
}								
int CVICALLBACK SPStart (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	unsigned char 	s_buf[20]={0};
	unsigned char	r_buf[8200]={0};
	double		 	buf[8200]={0};
	int num0,num,j,i,r_num,order,ii;
	int comflag=0;
	int 	com,avnum;
	char 	msg[200];
	int		ph;
	
	double unit;
	
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal(panel,SPPANEL_SPLEN,	&Sampling.splen);
			GetCtrlVal(panel,SPPANEL_SPFREQ,&Sampling.spfreq);
			GetCtrlVal(panel,SPPANEL_TGLEN,	&Sampling.tglen);
			GetCtrlVal(panel,SPPANEL_TGVOL,	&Sampling.tgvol);
			GetCtrlVal(panel,SPPANEL_VUNIT,	&Sampling.vunit);	 
			GetCtrlVal(panel,SPPANEL_AUNIT,	&Sampling.aunit);
			GetCtrlVal(panel,SPPANEL_AVNUM,	&avnum);
			GetCtrlVal(panel,SPPANEL_CGQ,	    &Sampling.cgq);
			
			SetCtrlAttribute (panel,SPPANEL_TEST ,  ATTR_DIMMED, 1);
			SetCtrlAttribute (panel,SPPANEL_SAMPLE ,ATTR_DIMMED, 1);
			SetCtrlAttribute (panel,SPPANEL_STOP   ,ATTR_DIMMED, 0);
			SetCtrlAttribute (panel,SPPANEL_QUIT ,  ATTR_DIMMED, 1);

			GetCtrlVal(panel,SPPANEL_COM,	&com);
			sprintf(scom,"COM%i",com);
			OpenComConfig (com, scom, 57600, 0, 8, 1, 512, 512);
			SetComTime (com, 0.1);
			FlushInQ (com);
			
			ii=0;
			while(ii<avnum)
			{
				s_buf[0] 	= 0x20;
				s_buf[1]	= (Sampling.splen)&0xff;
				s_buf[2] 	= ((Sampling.splen)&0xff00)>>8;
				i			= 0.5+1500000/(Sampling.spfreq);
				s_buf[3]	= i&0xff;
				s_buf[4] 	= (i&0xff00)>>8;
				s_buf[5]	= (Sampling.tglen)&0xff;
				s_buf[6] 	= ((Sampling.tglen)&0xff00)>>8;
				s_buf[7]	= Sampling.tgvol;
				s_buf[8] 	= 0x30;
				s_buf[9]	= 0x00;
				s_buf[10]	= 0xaa;
				FlushInQ (com);
				ComWrt	(com, s_buf ,11);									   
				i=0;
				while(1)
				{
			          if(GetUserEvent (0, &ph, &control))
			          {
			            if(control == SPPANEL_STOP)
			            {
			              SetCtrlAttribute (ph,SPPANEL_TEST ,  ATTR_DIMMED, 0);
			              SetCtrlAttribute (ph,SPPANEL_SAMPLE ,ATTR_DIMMED, 0);
			              SetCtrlAttribute (ph,SPPANEL_STOP   ,ATTR_DIMMED, 1);
			              SetCtrlAttribute (ph,SPPANEL_QUIT ,  ATTR_DIMMED, 0);
			              CloseCom (com);
			              return 1;
			            }
			          }
					r_num=ComRd(com, r_buf+i, 512);
					i+=r_num;
					if(i>=2*(Sampling.splen)+3)
					{
						for(i=0;i<Sampling.splen;i++)
						{
							order = 1<<((r_buf[2*i+1]&0xf0)>>4);
							Wave.Y[i]=	Wave.Yt[i]= (r_buf[2*i]+(0x0f&r_buf[2*i+1])*256.0-2048)*0.00244140625/order ;
						}
						if(Sampling.cgq==1)  
						{
							for(i=0;i<Sampling.splen;i++)
								Wave.Y[i]=980.0*Wave.Yt[i]*20000/Sampling.aunit;
							Integrate (Wave.Y, Wave.len, 1.0/Sampling.spfreq, 0.0, 0.0, Wave.Yt);
							Copy1D(Wave.Yt,Wave.len,Wave.Y);
						}
						else
						{
							for(i=0;i<Sampling.splen;i++)
								Wave.Yt[i]=Wave.Y[i]=100.0*Wave.Yt[i]/Sampling.vunit;
							Copy1D(Wave.Yt,Wave.len,Wave.Y);
						}				 
						Wave.len = Sampling.splen;
						DrawWT(1);DrawWF(.5);
						sprintf(msg, "保留第%i次采样结果？",	
									 ii+1);
						if(1==ConfirmPopup ("提示信息", msg))
						{
							ii++;
							for(i=0;i<MAXLEN;i++)
									buf[i]+=Wave.Y[i];
						}
						comflag = 1;	//MessagePopup ("提示信息", "数据教授完毕，通讯结束！");
						break;
					}
				}
			}
			Wave.zoom=1;
			for(i=0;i<Sampling.splen;i++)  Wave.Y[i]=Wave.Yt[i]=buf[i]/avnum;
			Wave.len		= Sampling.splen;
			InitZT();
			mode = 0;
			SetCtrlVal (phMain, MAINPANEL_BINARYSWITCH,	mode);
			DrawWT(1);
			DrawWF(.5);
			CloseCom (com);
			DiscardPanel(panel);
			break;
		}
	return 0;
}

//******************************************************************//
//						缺陷面板
//******************************************************************//
int CVICALLBACK FaultQuit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DiscardPanel (phFault);
			break;
		}
	return 0;
}


int CVICALLBACK ChangeWaveColor (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			break;
		}
	return 0;
}

int CVICALLBACK FScopeCallback (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	double 	dx,dy;
	char 	msg[200];
	switch (event)
	{
		case EVENT_COMMIT: 
			break;
		case EVENT_VAL_CHANGED:
			GetGraphCursor (phMain, MAINPANEL_FSCOPE, 1,&dx ,&dy );
			sprintf(msg, "频率:%5.2fHz	幅值:%5.2f", 
					dx,100.0*Wave.Yf[(int)dx*Wave.len/Sampling.spfreq ]);
    		SetCtrlVal(phMain, MAINPANEL_FREQMSG, msg);
			break;									   
	}
	return 0;
}

int CVICALLBACK ChangFS (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	double i;
	switch (event)
	{
		case EVENT_COMMIT:
			GetCtrlVal (phMain, MAINPANEL_FSCALE,	&i);
			SetAxisRange (	phMain, MAINPANEL_FSCOPE, 
					VAL_MANUAL,0,i*Sampling.spfreq,
					VAL_NO_CHANGE,0,0);
			break;
		default:
			break;
	}
	return 0;
}

int CVICALLBACK ChangTS (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	double i;
	switch (event)
	{
		case EVENT_COMMIT:
			GetCtrlVal (phMain, MAINPANEL_TSCALE, 	&i);
			SetAxisRange (	phMain, MAINPANEL_TSCOPE, 
					VAL_MANUAL,0, i*Wave.len*1000/Sampling.spfreq,
					VAL_NO_CHANGE,0,0);
			break;
	}
	return 0;
}

int CVICALLBACK QQuit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
	{
		case EVENT_COMMIT:
			QuitUserInterface (0);
			break;
	}
	return 0;
}

int CVICALLBACK Filter (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
	{
		case EVENT_COMMIT:
			phFilter = LoadPanel (phMain, "reflect.uir", FiltePANEL);
			GetDefault();
			SetCtrlVal(phFilter,FiltePANEL_TYPE,	Default.buf.Item.fttype);
			SetCtrlVal(phFilter,FiltePANEL_ORDER,	Default.buf.Item.ftorder);
			SetCtrlVal(phFilter,FiltePANEL_FC,	Default.buf.Item.ftfcl);
			SetCtrlVal(phFilter,FiltePANEL_FC1,	Default.buf.Item.ftfch);
			DisplayPanel(phFilter);
			break;
	}
	return 0;
}

int CVICALLBACK ChangeType (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
					   
	int type;
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal (phFilter, FiltePANEL_TYPE,	&type);
			if(type>=2)		SetCtrlAttribute (phFilter, FiltePANEL_FC1, ATTR_DIMMED, 0);
			else			SetCtrlAttribute (phFilter, FiltePANEL_FC1, ATTR_DIMMED, 1); 
			break;
		}
	return 0;
}

int CVICALLBACK FilterOK (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	IIRFilterPtr ftInfo;
	int	type,order,i;
	double fs,fh,fl;
	
	switch (event)
	{
		case EVENT_COMMIT:
	
			fs=Sampling.spfreq;
			GetCtrlVal (phFilter, FiltePANEL_TYPE,	&type);
			GetCtrlVal (phFilter, FiltePANEL_ORDER,	&order);
			GetCtrlVal (phFilter, FiltePANEL_FC,	&fl);
			GetCtrlVal (phFilter, FiltePANEL_FC1,	&fh);
	
			ftInfo = AllocIIRFilterPtr (type, order);
			Bw_CascadeCoef (fs,fl,fh ,ftInfo);
			IIRCascadeFiltering (Wave.Y, Wave.len,ftInfo ,Wave.Yt );
			FreeIIRFilterPtr (ftInfo);
			
			UpdataWT();
			UpdataWF();
			break;
	}
	return 0;
}

int CVICALLBACK FilterCancel (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
	{
		case EVENT_COMMIT:
			//DiscardPanel (phFilter);
			DiscardPanel(GetActivePanel ());
			break;
	}
	return 0;
}

int CVICALLBACK FilterResume (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
	{
		case EVENT_COMMIT:
			Wave.zoom=1;
			for(i=0;i<MAXLEN;i++)		Wave.Yt[i]	=Wave.Y[i];
			UpdataWT();
			UpdataWF();
			break;
		default:
			break;
	}
	return 0;
}

int CVICALLBACK Invert (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
	{
		case EVENT_COMMIT:
			for(i=0;i<Wave.len;i++)
			{
				Wave.Yt[i]=-1*Wave.Yt[i];
				Wave.Y[i]=-1*Wave.Y[i];
			}
			UpdataWT();
			UpdataWF();
			break;
	}
	return 0;
}

int CVICALLBACK Zoom (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{   
	char	input[10];
	double	a=1,f;												  
	int i;
	char  msg[100];
	
	switch (event)
	{
		case EVENT_COMMIT:
			PromptPopup (	"指数放大",
   							"放大倍数",
   							 input,4);
			Fmt(&a,"%f<%s",input);
			f = log(a/Wave.zoom); 
			Wave.zoom=a;		  
			for(i=0;i<Wave.len;i++)		
				Wave.Yt[i]	=Wave.Yt[i]*exp(f*i/Wave.len);
			UpdataWT();
			UpdataWF();
			DrawZT();
			break;
	}
	return 0;
}

int CVICALLBACK ChangMode (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			GetCtrlVal (phMain, MAINPANEL_BINARYSWITCH,	&mode);
			break;
		}
	return 0;
}

int CVICALLBACK Sample (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int num0;
	switch (event)
		{
		case EVENT_COMMIT:
			phSampling = LoadPanel (phMain, "reflect.uir", SPPANEL);
			GetDefault();
			SetCtrlVal(phSampling,SPPANEL_SPLEN,	Default.buf.Item.splen);
			SetCtrlVal(phSampling,SPPANEL_SPFREQ,	Default.buf.Item.spfreq);
			SetCtrlVal(phSampling,SPPANEL_AVNUM,	Default.buf.Item.spnum);
			SetCtrlVal(phSampling,SPPANEL_TGLEN,	Default.buf.Item.sptaglen);
			
			SetCtrlVal(phSampling,SPPANEL_TGVOL,	Default.buf.Item.sptagvol);
			SetCtrlVal(phSampling,SPPANEL_CGQ,		Default.buf.Item.spcgq);
			
			SetCtrlVal(phSampling,SPPANEL_AUNIT,		Default.buf.Item.fxaunit);
			SetCtrlVal(phSampling,SPPANEL_VUNIT,		Default.buf.Item.fxvunit);
			
			SetCtrlVal(phSampling,SPPANEL_WAITNUMERIC,	Default.buf.Item.spwtime);
			SetCtrlVal(phSampling,SPPANEL_COM,		Default.buf.Item.spcom);
			InstallPopup (phSampling);
			break;
		}
	return 0;
}

int CVICALLBACK ChangeRate (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DrawZT();
			break;
		}
	return 0;
}
//List

static void popFaultType (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int 	index;
	GetCtrlIndex (phMain, MAINPANEL_LISTBOX, &index);
	index--;
	if(index<=Pole.faultnum)
	{
		Pole.fault[index].type = menuItem-9;
		DrawZT();
	}
	index++;
	SetCtrlIndex (phMain, MAINPANEL_LISTBOX, index);
}
static void popFlautLevel (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int 	index;
	GetCtrlIndex (phMain, MAINPANEL_LISTBOX, &index);
	index--;
	if(index<=Pole.faultnum)
	{
		Pole.fault[index].level = menuItem-14;
		DrawZT();
	}
	index++;
	SetCtrlIndex (phMain, MAINPANEL_LISTBOX, index);
}

static void popFaultDelect (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int 	index,i;
	GetCtrlIndex (phMain, MAINPANEL_LISTBOX, &index);
	index--;
	for(i=index;i<=Pole.faultnum;i++)
	{
		Pole.fault[i].pos 	= Pole.fault[i+1].pos;
		Pole.fault[i].type 	= Pole.fault[i+1].type;
		Pole.fault[i].level = Pole.fault[i+1].level;
	}
	Pole.faultnum--;
	DrawZT();
	index++;
	SetCtrlIndex (phMain, MAINPANEL_LISTBOX, index-1);
}

static void popBottomType (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	Pole.bottom.type = menuItem-3;
	DrawZT();
}

int CVICALLBACK ChangeList (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int 	X,Y;
	int 	index;
	int 	popmenubar;
	int		popmenu,typemenu,levelmenu;
	int		m1,m2;
	
	switch (event)
		{
		case EVENT_COMMIT:
			break;
		case EVENT_RIGHT_CLICK:
			if(mode==0) break;
		    GetRelativeMouseState (panel, 0, &X, &Y, NULL,NULL,NULL);
			GetCtrlIndex (phMain, MAINPANEL_LISTBOX, &index);
			if(index>1 && index<=Pole.faultnum+1)
			{
				popmenubar  = NewMenuBar (0);
				popmenu		= NewMenu (popmenubar, "TOP", -1);
				
				m1=NewMenuItem (popmenubar, popmenu, "类型",	-1, 0, popFaultDelect,0);
				m2=NewMenuItem (popmenubar, popmenu, "程度",	-1, 0, popFaultDelect,0);
				InsertSeparator (popmenubar,popmenu ,-1 );
				NewMenuItem (popmenubar, popmenu, "删除",	-1, 0, popFaultDelect,0);
				typemenu	=NewSubMenu (popmenubar,	m1);
				levelmenu	=NewSubMenu (popmenubar,	m2);

				NewMenuItem (popmenubar, typemenu, "离析",	-1, 0, popFaultType, 0);
				NewMenuItem (popmenubar, typemenu, "断裂",	-1, 0, popFaultType, 0);
				NewMenuItem (popmenubar, typemenu, "加泥",	-1, 0, popFaultType, 0);
				NewMenuItem (popmenubar, typemenu, "缩颈",	-1, 0, popFaultType, 0);
				NewMenuItem (popmenubar, typemenu, "扩颈",	-1, 0, popFaultType, 0);
				
				NewMenuItem (popmenubar, levelmenu, "轻微",	-1, 0, popFlautLevel,0);  
				NewMenuItem (popmenubar, levelmenu, "明显",	-1, 0, popFlautLevel,0);  
				NewMenuItem (popmenubar, levelmenu, "严重",	-1, 0, popFlautLevel,0);  
				RunPopupMenu (popmenubar, popmenu, panel, Y,X, 0, 0, 0, 0);
				DiscardMenuBar (popmenubar);
			}
			if(index==1)
			{
				popmenubar  = NewMenuBar (0);
				popmenu   = NewMenu (popmenubar, "Bottom", -1);      
				NewMenuItem (popmenubar, popmenu, "平底",		-1, 0, popBottomType, 0);
				NewMenuItem (popmenubar, popmenu, "尖底",		-1, 0, popBottomType, 0);
				NewMenuItem (popmenubar, popmenu, "扩底",		-1, 0, popBottomType, 	0);  
				RunPopupMenu (popmenubar, popmenu, panel, Y,X, 0, 0, 0, 0);
				DiscardMenuBar (popmenubar);
			}
			break;
 		default:											
			break;
		}
	return 0;
}


//****************************************************************

void CVICALLBACK miPrintCurrent (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	PrintReport(0);
}

void CVICALLBACK miPrint (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	PrintReport(1);
}

void CVICALLBACK menuPrint1 (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	SetPrintAttribute (ATTR_ORIENTATION, VAL_PORTRAIT);
   	SetPrintAttribute (ATTR_PRINT_AREA_HEIGHT, VAL_USE_ENTIRE_PAPER);
   	SetPrintAttribute (ATTR_PRINT_AREA_WIDTH, VAL_INTEGRAL_SCALE);
   	PrintPanel (phPrint, "", 1, VAL_VISIBLE_AREA, 0);
}
	

void CVICALLBACK menuPrint2 (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	DiscardPanel(phPrint);     
}

int CVICALLBACK PrintExit (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
			DiscardPanel(phPrint);     
			break;
		}
	return 0;
}

int CVICALLBACK PrintSet (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	char msg[200];
	switch (event)
		{
		case EVENT_COMMIT:
			switch(control-PRINTPANEL_GRAPH)
			{
			case 0: SetCtrlVal (phPrint, PRINTPANEL_MSG, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE, 		Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE, 		Print.date);
					break;
			case 1: SetCtrlVal (phPrint, PRINTPANEL_MSG_2, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_2, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_2, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_2, 	Print.date);
					break;
			case 2: SetCtrlVal (phPrint, PRINTPANEL_MSG_3, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_3, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_3, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_3, 	Print.date);
					break;
			case 3: SetCtrlVal (phPrint, PRINTPANEL_MSG_4, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_4, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_4, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_4, 	Print.date);
					break;
			case 4: SetCtrlVal (phPrint, PRINTPANEL_MSG_5, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_5, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_5, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_5, 	Print.date);
					break;
			}		
			break;
		}
	return 0;
}
//miAbout
void CVICALLBACK miAbout (int menuBar, int menuItem, void *callbackData,
		int panel)
{
	int phAbout=LoadPanel(phMain, "reflect.uir",ABOUTPANEL);
	InstallPopup (phAbout);									
}

int CVICALLBACK AboutOK (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
		    DiscardPanel(GetActivePanel ());
			break;
		}
	return 0;
}


int CVICALLBACK btnPrint (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	if(event!=EVENT_LEFT_CLICK) return 1;
	PrintReport(1);
	return 0;
}


int 	PrintReport(int pmode)
{
	char **	FileSet;
	double 	Xmin,Xmax,Ymin,Ymax;
	int 	Xmode,Ymode;
	double 	max,min;
	int 	imax,imin;
	double 	x1,y1,x2,y2;
	double 	X[3],Y[3];
	double  ZC;		  
	char 	level[10],type[10],msg[300],s1[100];
	double 	v0,len0,t;
	char 	FileName[300],DirPath[500];
	int 	FileNum;
	double 	x[4096];
	int		WavePad;
	int 	i,row;
	double  Ts;
	
	if(pmode == 1)
	{									 
		//Get Print File;
		GetDir (DirPath);
		sprintf(DirPath,	"%s\\data\\*.dat",DirPath);					         
		sprintf(FileName,	"%s","*.dat");
		MultiFileSelectPopup (DirPath, FileName, "*.dat", "打印文件选择", 0, 1, 1,
							  &FileNum, &FileSet);								  
		if(FileNum>5 || FileNum<=0) return 1;
	}
	else
	  FileNum=1;

	//Initial Form;
	phPrint= LoadPanel (0, "reflect.uir", PRINTPANEL);
	Ts=1000.0/Sampling.spfreq;
	for(i=0;i<Wave.len;i++)	x[i]=Ts*i;
	
	//Darw graphic
	for(row=0;row<5;row++)
	{
		WavePad=PRINTPANEL_GRAPH+row;
		GetAxisRange (phPrint, WavePad,&Xmode, &Xmin, &Xmax, &Ymode, &Ymin, &Ymax);
		PlotRectangle (phPrint, WavePad, Xmin, Ymin, Xmax, Ymax, VAL_BLACK,VAL_WHITE);
		PlotLine (phPrint, WavePad, 0, 0, 	 Xmax, 0,VAL_BLACK);
		
		if(row<FileNum)
		{
			if(pmode==1) Open(FileSet[row]);
			//Set Cooration
			MaxMin1D (Wave.Yt, Wave.len, &max, &imax, &min, &imin);max= ((max<-min)?-min:max);
			SetAxisRange (phPrint, WavePad, VAL_MANUAL, 0.0,x[Wave.len-1],VAL_MANUAL, -1.6*max, max);
			GetAxisRange (phPrint, WavePad,&Xmode, &Xmin, &Xmax, &Ymode, &Ymin, &Ymax);
			PlotRectangle (phPrint, WavePad, Xmin, Ymin, Xmax, Ymax, VAL_BLACK,VAL_WHITE);
			PlotLine (phPrint, WavePad, 0, 0, 	 Xmax, 0,VAL_BLACK);
			//Wave
			PlotXY (phPrint, WavePad, x, Wave.Yt, Wave.len, VAL_DOUBLE,VAL_DOUBLE, VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1,VAL_BLACK);
			//ZT
			PlotRectangle (phPrint, WavePad,Pole.top,-1.1*max,Pole.bottom.pos,-1.4*max,VAL_BLACK, VAL_WHITE);
			//虚线
			X[0] = Pole.top;	Y[0] = max;
			X[1] = Pole.top;	Y[1] = -1.50*max;
			PlotXY (phPrint, WavePad, X, Y, 2, VAL_DOUBLE, VAL_DOUBLE,VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_DOT, 1, VAL_BLACK);
			X[0] = Pole.bottom.pos;	Y[0] = max;
			X[1] = Pole.bottom.pos;	Y[1] = -1.50*max;
			PlotXY (phPrint, WavePad, X, Y, 2, VAL_DOUBLE, VAL_DOUBLE,VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_DOT, 1, VAL_BLACK);
			//指数放大
			sprintf(msg, "指数放大倍数:  %i",
							Wave.zoom);
			//桩底		
			switch(Pole.bottom.type)
			{
				case 0: sprintf(msg, "%s\n类型 ： 平底桩",msg);
						break;
				case 1: sprintf(msg, "%s\n类型 ： 尖底桩",msg);
						X[0] = Pole.bottom.pos*0.99;	Y[0] = -1.10*max;
						X[1] = Pole.bottom.pos*0.99;	Y[1] = -1.40*max;
						X[2] = Pole.bottom.pos*1.01;	Y[2] = -1.25*max;
						PlotPolygon (phPrint, WavePad,X,Y,3,VAL_DOUBLE,VAL_DOUBLE,VAL_BLACK,VAL_WHITE);
						break;
				case 2:	sprintf(msg, "%s\n类型 ： 扩底桩",msg);
						X[0] = Pole.bottom.pos*0.99;	Y[0] = -1.00*max;
						X[1] = Pole.bottom.pos*1.01;	Y[1] = -1.50*max;
						PlotRectangle (phPrint, WavePad,X[0],Y[0],X[1],Y[1],VAL_BLACK,VAL_WHITE);
						break;
				default:sprintf(msg, "%s\n类型 ： 平底桩",msg);
						break;									  
			}
			GetCtrlVal (phMain, MAINPANEL_RATE,		&v0);
			GetCtrlVal (phMain, MAINPANEL_LENGTH,	&len0);
			sprintf(msg, "%s\n波速 ： %4.2fm",	
					msg, v0);
			t=(Pole.bottom.pos - Pole.top)/1000.0;					 
			sprintf(msg, "%s\n桩长 ： %4.2fm", 		
					msg,v0*t*0.5);
			//桩体缺陷
			ZC=(Pole.bottom.pos-Pole.top)/100.0;
			for(i=1;i<=Pole.faultnum;i++)
			{   		
				PlotRectangle (phPrint, WavePad,
							   Pole.fault[i].pos-ZC,		-1.1*max,
							   Pole.fault[i].pos+ZC,		-1.4*max, 
							   VAL_BLACK, VAL_LT_GRAY);
				sprintf(s1, "%i",i);		  
				PlotText (	phPrint, WavePad,
							Pole.fault[i].pos-ZC,		-1.05*max,
							s1, VAL_APP_META_FONT, VAL_BLACK,VAL_TRANSPARENT);
				//箭头
				X[0] = Pole.fault[i].pos;	Y[0] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)];
				X[1] = Pole.fault[i].pos;	Y[1] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)]+0.3*max;
				PlotXY (phPrint, WavePad, X, Y, 2, VAL_DOUBLE, VAL_DOUBLE,
						VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1, VAL_BLACK);
				X[0] = Pole.fault[i].pos;		Y[0] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)];
				X[1] = Pole.fault[i].pos-ZC;	Y[1] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)]+0.1*max;
				PlotXY (phPrint, WavePad, X, Y, 2, VAL_DOUBLE, VAL_DOUBLE,
						VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1, VAL_BLACK);
				X[0] = Pole.fault[i].pos;		Y[0] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)];
				X[1] = Pole.fault[i].pos+ZC;	Y[1] = Wave.Yt[(int)(Pole.fault[i].pos/Ts)]+0.1*max;
				PlotXY (phPrint, WavePad, X, Y, 2, VAL_DOUBLE, VAL_DOUBLE,
						VAL_THIN_LINE, VAL_EMPTY_SQUARE, VAL_SOLID, 1, VAL_BLACK);
				//信息
				t=(Pole.fault[i].pos - Pole.top)/1000.0;
				GetFaultLevel(Pole.fault[i].level,	level);
				GetFaultType(Pole.fault[i].type,	type);
				sprintf(msg, "%s\n缺陷%i： %4.2fm %s%s",
						msg,i,v0*t*0.5,level,type);
			}
			//参数显示
			switch(row)
			{
			case 0: SetCtrlVal (phPrint, PRINTPANEL_MSG, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE, 		Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE, 		Print.date);
					break;
			case 1: SetCtrlVal (phPrint, PRINTPANEL_MSG_2, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_2, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_2, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_2, 	Print.date);
					break;
			case 2: SetCtrlVal (phPrint, PRINTPANEL_MSG_3, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_3, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_3, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_3, 	Print.date);
					break;
			case 3: SetCtrlVal (phPrint, PRINTPANEL_MSG_4, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_4, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_4, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_4, 	Print.date);
					break;
			case 4: SetCtrlVal (phPrint, PRINTPANEL_MSG_5, 		msg);
					SetCtrlVal (phPrint, PRINTPANEL_NUMBER_5, 	Print.no);
					SetCtrlVal (phPrint, PRINTPANEL_PLACE_5, 	Print.place);
					SetCtrlVal (phPrint, PRINTPANEL_DATE_5, 	Print.date);
					break;
			}		
		}
	}
	DisplayPanel (phPrint);
	return 0;
}

int CVICALLBACK ChangeCall (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	switch (event)
		{
		case EVENT_COMMIT:
		    

			break;
		}
	return 0;
}
