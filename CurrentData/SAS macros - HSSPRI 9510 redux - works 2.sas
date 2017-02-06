/*******************************************************************
      Time-Shifting Target - OP Hostility & Support Variables 
				      	(1995 - 2010)
********************************************************************/

options mprint formdlim='_' nocenter nodate; 
DM 'OUTPUT; CLEAR; LOG; CLEAR; ';

libname hs 'F:\Backup\Time Shifting';
data frmt;
set hs.hsspri9507;

proc format; 
value sex
	1 = "1: Male"
	2 = "2: Female";
value relstat
	1 = "1: Married"
	2 = "2: Cohabiting"
	3 = "3: Dating"
	4 = "4: Not Dating"
	5 = "5: Separated/divorced/widowed";
value samp
	1 = "1: IYFP"
	2 = "2: ISPP";
value datetype
	1 = "1: MAR" 
	2 = "2: COH";
value miss
	0 - high = "VALID"
	. = "SAMEOP/ITEMMISS" 
	.O = "OTHER PART"
	.N = "NO BIG OP"
	.U = "PARTNER ID UNKNOWN" 
	.B = "OUTOFRANGE/HIGH"
	.A = "OUTOFRANGE/LOW";
value miss_
	0 - high = "VALID"
	. = "ITEMMISS" 
	.N = "NO BIG OP"
	.B = "OUTOFRANGE/HIGH"
	.A = "OUTOFRANGE/LOW";
run;



data indices;
set frmt;

/*****************************************************************************************************************
	Macro MARCOHDAT combines variables from questionnaire 4 (married/cohabiting) with questionnaire 5 (dating)
    --> creates new "COMBINED" variable "FTc _ _ _ _ _ _" or "HTc_ _ _ _ _ _".
    (each of the new variable has range 1-7; 8, 9 = missing) 
  KB**Amendment: f17 changed qrre numbers so 3 (mar/coh) and 4 (dating); individual item ranges are identical**
*****************************************************************************************************************/
%macro marcohdat(combined,marcoh,dat);  /**marcohdat=name of macro**; combined=new name,marcoh=1st element; dat=2nd**/
if 1<=&marcoh<=7 then &combined=&marcoh;
else if 1<=&dat<=7 then &combined=&dat;
else &combined=.;
%mend; /**%mend=macro end**/
/**combines married & cohabiting with dating**/

/*************************************************************
	Macro MakeIndex takes average of non-missing values,
	names the variable and labels it as directed
**************************************************************/
%macro makeindex(rep/*reporter*/,con/*construct*/,init/*initiator*/,rec/*receiver*/,year,vars,high,title);
array arr&rep.r&init&con&rec&year(*) &vars;  /**high=highest possible value PER ITEM, not for total index**/
/*recode missing values*/
do i = 1 to dim(arr&rep.r&init&con&rec&year); 
	if 1<= arr&rep.r&init&con&rec&year(i)<= &high 
		then arr&rep.r&init&con&rec&year(i)=arr&rep.r&init&con&rec&year(i);
	else arr&rep.r&init&con&rec&year(i)=.; 
end;
/*average nonmissing values to form index*/
&rep.r&init&con&rec&year=mean (of &vars);
label &rep.r&init&con&rec&year="&title"; 
%mend; 


/****************************************************************************
   OBSERVER REPORT OF TARGET'S HOSTILITY/SUPPORT TOWARD PARTNER 1995-2010
*****************************************************************************/

%makeindex(O,host,T,P,95,FT245HSZ FT245ACZ FT245EHZ FT245RHZ FT245ANZ,9,Obs: T Host-->P 1995); 		/*Created variable: OrThostP95 */
%makeindex(O,host,T,P,97,FT445HSZ FT445ACZ FT445EHZ FT445RHZ FT445ANZ,9,Obs: T Host-->P 1997); 
%makeindex(O,host,T,P,99,FT645HSZ FT645ACZ FT645EHZ FT645RHZ FT645ANZ,9,Obs: T Host-->P 1999); 
%makeindex(O,host,T,P,01,FT845HSZ FT845ACZ FT845EHZ FT845RHZ FT845ANZ,9,Obs: T Host-->P 2001);
%makeindex(O,host,T,P,03,HT045HSZ HT045ACZ HT045EHZ HT045RHZ HT045ANZ,9,Obs: T Host-->P 2003);
%makeindex(O,host,T,P,05,HT205HSZ HT205ACZ HT205EHZ HT205RHZ HT205ANZ,9,Obs: T Host-->P 2005);
%makeindex(O,host,T,P,07,HT405HSZ HT405ACZ HT405EHZ HT405RHZ HT405ANZ,9,Obs: T Host-->P 2007);
/*%makeindex(O,host,T,P,10,HT705HSZ HT705ACZ HT705EHZ HT705RHZ HT705ANZ,9,Obs: T Host-->P 2010);*/

%makeindex(O,supp,T,P,95,FT245WMZ FT245EWZ FT245RWZ FT245ARZ FT245LRZ FT245COZ FT245PRZ,9,Obs: T Supp-->P 1995);
%makeindex(O,supp,T,P,97,FT445WMZ FT445EWZ FT445RWZ FT445ARZ FT445LRZ FT445COZ FT445PRZ,9,Obs: T Supp-->P 1997);
%makeindex(O,supp,T,P,99,FT645WMZ FT645EWZ FT645RWZ FT645ARZ FT645LRZ FT645COZ FT645PRZ,9,Obs: T Supp-->P 1999);
%makeindex(O,supp,T,P,01,FT845WMZ FT845EWZ FT845RWZ FT845ARZ FT845LRZ FT845COZ FT845PRZ,9,Obs: T Supp-->P 2001);
%makeindex(O,supp,T,P,03,HT045WMZ HT045EWZ HT045RWZ HT045ARZ HT045LRZ HT045COZ HT045PRZ,9,Obs: T Supp-->P 2003);
%makeindex(O,supp,T,P,05,HT205WMZ HT205EWZ HT205RWZ HT205ARZ HT205LRZ HT205COZ HT205PRZ,9,Obs: T Supp-->P 2005);
%makeindex(O,supp,T,P,07,HT405WMZ HT405EWZ HT405RWZ HT405ARZ HT405LRZ HT405COZ HT405PRZ,9,Obs: T Supp-->P 2007);
/*%makeindex(O,supp,T,P,10,HT705WMZ HT705EWZ HT705RWZ HT705ARZ HT705LRZ HT705COZ HT705PRZ,9,Obs: T Supp-->P 2010);*/

/*proc freq; 
tables OrThostP95*FT245HSZ*FT245ACZ*FT245EHZ*FT245RHZ*FT245ANZ/list missing; 
run;*/


/***************************************************************************
   OBSERVER REPORT OF PARTNER'S HOSTILITY/SUPPORT TOWARD TARGET 1995-2010
****************************************************************************/

%makeindex(O,host,P,T,95,FZ245HST FZ245ACT FZ245EHT FZ245RHT FZ245ANT,9,Obs: P Host-->T 1995); 		/*Created variable: OrPhostT95 */
%makeindex(O,host,P,T,97,FZ445HST FZ445ACT FZ445EHT FZ445RHT FZ445ANT,9,Obs: P Host-->T 1997); 
%makeindex(O,host,P,T,99,FZ645HST FZ645ACT FZ645EHT FZ645RHT FZ645ANT,9,Obs: P Host-->T 1999); 
%makeindex(O,host,P,T,01,FZ845HST FZ845ACT FZ845EHT FZ845RHT FZ845ANT,9,Obs: P Host-->T 2001);
%makeindex(O,host,P,T,03,HZ045HST HZ045ACT HZ045EHT HZ045RHT HZ045ANT,9,Obs: P Host-->T 2003);
%makeindex(O,host,P,T,05,HZ205HST HZ205ACT HZ205EHT HZ205RHT HZ205ANT,9,Obs: P Host-->T 2005);
%makeindex(O,host,P,T,07,HZ405HST HZ405ACT HZ405EHT HZ405RHT HZ405ANT,9,Obs: P Host-->T 2007);
/*%makeindex(O,host,P,T,10,HZ705HST HZ705ACT HZ705EHT HZ705RHT HZ705ANT,9,Obs: P Host-->T 2010);*/

%makeindex(O,supp,P,T,95,FZ245WMT FZ245EWT FZ245RWT FZ245ART FZ245LRT FZ245COT FZ245PRT,9,Obs: P Supp-->T 1995);
%makeindex(O,supp,P,T,97,FZ445WMT FZ445EWT FZ445RWT FZ445ART FZ445LRT FZ445COT FZ445PRT,9,Obs: P Supp-->T 1997);
%makeindex(O,supp,P,T,99,FZ645WMT FZ645EWT FZ645RWT FZ645ART FZ645LRT FZ645COT FZ645PRT,9,Obs: P Supp-->T 1999);
%makeindex(O,supp,P,T,01,FZ845WMT FZ845EWT FZ845RWT FZ845ART FZ845LRT FZ845COT FZ845PRT,9,Obs: P Supp-->T 2001);
%makeindex(O,supp,P,T,03,HZ045WMT HZ045EWT HZ045RWT HZ045ART HZ045LRT HZ045COT HZ045PRT,9,Obs: P Supp-->T 2003);
%makeindex(O,supp,P,T,05,HZ205WMT HZ205EWT HZ205RWT HZ205ART HZ205LRT HZ205COT HZ205PRT,9,Obs: P Supp-->T 2005);
%makeindex(O,supp,P,T,07,HZ405WMT HZ405EWT HZ405RWT HZ405ART HZ405LRT HZ405COT HZ405PRT,9,Obs: P Supp-->T 2007);
/*%makeindex(O,supp,P,T,10,HZ705WMT HZ705EWT HZ705RWT HZ705ART HZ705LRT HZ705COT HZ705PRT,9,Obs: P Supp-->T 2010);*/


/*******************************************************************
   PARTNER REPORT OF TARGET'S HOSTILITY TOWARD PARTNER 1995-2010
********************************************************************/

%marcohdat(FZc241043, FZ241043, FZ251040);  /**Create new variable first: FZc is new var, FZs are mar/coh & dating**/
%marcohdat(FZc241047, FZ241047, FZ251044); 
%marcohdat(FZc241048, FZ241048, FZ251045); 
%marcohdat(FZc241056, FZ241056, FZ251053); 
%marcohdat(FZc241058, FZ241058, FZ251055); 
%makeindex(P,host,T,P,95,FZc241043 FZc241047 FZc241048 FZc241056 FZc241058,7,Part: T Host-->P 1995);  /**creates average index (burgundy in file)**/

%marcohdat(FZc441045, FZ441045, FZ451040);
%marcohdat(FZc441049, FZ441049, FZ451044);
%marcohdat(FZc441050, FZ441050, FZ451045);
%marcohdat(FZc441058, FZ441058, FZ451053);
%marcohdat(FZc441060, FZ441060, FZ451055); 
%makeindex(P,host,T,P,97,FZc441045 FZc441049 FZc441050 FZc441058 FZc441060,7,Part: T Host-->P 1997);

%marcohdat(FZc641045, FZ641045, FZ651040); 
%marcohdat(FZc641049, FZ641049, FZ651044);
%marcohdat(FZc641050, FZ641050, FZ651045);
%marcohdat(FZc641058, FZ641058, FZ651053);
%marcohdat(FZc641060, FZ641060, FZ651055);
%makeindex(P,host,T,P,99,FZc641045 FZc641049 FZc641050 FZc641058 FZc641060,7,Part: T Host-->P 1999);

%marcohdat(FZc841047, FZ841047, FZ851042); 
%marcohdat(FZc841051, FZ841051, FZ851046);
%marcohdat(FZc841052, FZ841052, FZ851047); 
%marcohdat(FZc841060, FZ841060, FZ851055); 
%marcohdat(FZc841062, FZ841062, FZ851057); 
%makeindex(P,host,T,P,01,FZc841047 FZc841051 FZc841052 FZc841060 FZc841062,7,Part: T Host-->P 2001);

%marcohdat(HZc041044, HZ041044, HZ051043);
%marcohdat(HZc041048, HZ041048, HZ051047);
%marcohdat(HZc041049, HZ041049, HZ051048);
%marcohdat(HZc041057, HZ041057, HZ051056);
%marcohdat(HZc041059, HZ041059, HZ051058);
%makeindex(P,host,T,P,03,HZc041044 HZc041048 HZc041049 HZc041057 HZc041059,7,Part: T Host-->P 2003);

%marcohdat(HZc241041, HZ241041, HZ251040);
%marcohdat(HZc241045, HZ241045, HZ251044);
%marcohdat(HZc241046, HZ241046, HZ251045);
%marcohdat(HZc241054, HZ241054, HZ251053);
%marcohdat(HZc241056, HZ241056, HZ251055);
%makeindex(P,host,T,P,05,HZc241041 HZc241045 HZc241046 HZc241054 HZc241056,7,Part: T Host-->P 2005);

%marcohdat(HZc441044, HZ441044, HZ451040);
%marcohdat(HZc441048, HZ441048, HZ451044);
%marcohdat(HZc441049, HZ441049, HZ451045);
%marcohdat(HZc441057, HZ441057, HZ451053);
%marcohdat(HZc441059, HZ441059, HZ451055);
%makeindex(P,host,T,P,07,HZc441044 HZc441048 HZc441049 HZc441057 HZc441059,7,Part: T Host-->P 2007);

/*%marcohdat(HZc731025, HZ731025, HZ741022);
%marcohdat(HZc731029, HZ731029, HZ741026);
%marcohdat(HZc731030, HZ731030, HZ741027);
%marcohdat(HZc731038, HZ731038, HZ741035);
%marcohdat(HZc731040, HZ731040, HZ741037);
%makeindex(P,host,T,P,10,HZc731025 HZc731029 HZc731030 HZc731038 HZc731040,7,Part: T Host-->P 2010);*/


/******************************************************************
   PARTNER REPORT OF TARGET'S SUPPORT TOWARD PARTNER 1995-2010
*******************************************************************/

%marcohdat(FZc241044, FZ241044, FZ251041); 
%marcohdat(FZc241045, FZ241045, FZ251042); 
%marcohdat(FZc241046, FZ241046, FZ251043); 
%marcohdat(FZc241049, FZ241049, FZ251046); 
%marcohdat(FZc241052, FZ241052, FZ251049); 
%marcohdat(FZc241053, FZ241053, FZ251050); 
%marcohdat(FZc241059, FZ241059, FZ251056); 
%marcohdat(FZc241064, FZ241064, FZ251061); 
%makeindex(P,supp,T,P,95,FZc241044 FZc241045 FZc241046 FZc241049 FZc241052 FZc241053 FZc241059 FZc241064,7,Part: T Supp-->P 1995);

%marcohdat(FZc441046, FZ441046, FZ451041);
%marcohdat(FZc441047, FZ441047, FZ451042);
%marcohdat(FZc441048, FZ441048, FZ451043);
%marcohdat(FZc441051, FZ441051, FZ451046);
%marcohdat(FZc441054, FZ441054, FZ451049);
%marcohdat(FZc441055, FZ441055, FZ451050);
%marcohdat(FZc441061, FZ441061, FZ451056);
%marcohdat(FZc441066, FZ441066, FZ451061);
%makeindex(P,supp,T,P,97,FZc441046 FZc441047 FZc441048 FZc441051 FZc441054 FZc441055 FZc441061 FZc441066,7,Part: T Supp-->P 1997);

%marcohdat(FZc641046, FZ641046, FZ651041);
%marcohdat(FZc641047, FZ641047, FZ651042);
%marcohdat(FZc641048, FZ641048, FZ651043);
%marcohdat(FZc641051, FZ641051, FZ651046);
%marcohdat(FZc641054, FZ641054, FZ651049);
%marcohdat(FZc641055, FZ641055, FZ651050);
%marcohdat(FZc641061, FZ641061, FZ651056);
%marcohdat(FZc641066, FZ641066, FZ651061);
%makeindex(P,supp,T,P,99,FZc641046 FZc641047 FZc641048 FZc641051 FZc641054 FZc641055 FZc641061 FZc641066,7,Part: T Supp-->P 1999);

%marcohdat(FZc841048, FZ841048, FZ851043);
%marcohdat(FZc841049, FZ841049, FZ851044);
%marcohdat(FZc841050, FZ841050, FZ851045);
%marcohdat(FZc841053, FZ841053, FZ851048);
%marcohdat(FZc841056, FZ841056, FZ851051);
%marcohdat(FZc841057, FZ841057, FZ851052);
%marcohdat(FZc841063, FZ841063, FZ851058);
%marcohdat(FZc841068, FZ841068, FZ851063);
%makeindex(P,supp,T,P,01,FZc841048 FZc841049 FZc841050 FZc841053 FZc841056 FZc841057 FZc841063 FZc841068,7,Part: T Supp-->P 2001);

%marcohdat(HZc041045, HZ041045, HZ051044);
%marcohdat(HZc041046, HZ041046, HZ051045);
%marcohdat(HZc041047, HZ041047, HZ051046);
%marcohdat(HZc041050, HZ041050, HZ051049);
%marcohdat(HZc041053, HZ041053, HZ051052);
%marcohdat(HZc041054, HZ041054, HZ051053);
%marcohdat(HZc041060, HZ041060, HZ051059);
%marcohdat(HZc041065, HZ041065, HZ051064);
%makeindex(P,supp,T,P,03,HZc041045 HZc041046 HZc041047 HZc041050 HZc041053 HZc041054 HZc041060 HZc041065,7,Part: T Supp-->P 2003);

%marcohdat(HZc241042, HZ241042, HZ251041);
%marcohdat(HZc241043, HZ241043, HZ251042); 
%marcohdat(HZc241044, HZ241044, HZ251043);
%marcohdat(HZc241047, HZ241047, HZ251046);
%marcohdat(HZc241050, HZ241050, HZ251049);
%marcohdat(HZc241051, HZ241051, HZ251050);
%marcohdat(HZc241057, HZ241057, HZ251056);
%marcohdat(HZc241062, HZ241062, HZ251061);
%makeindex(P,supp,T,P,05,HZc241042 HZc241043 HZc241044 HZc241047 HZc241050 HZc241051 HZc241057 HZc241062,7,Part: T Supp-->P 2005);

%marcohdat(HZc441045, HZ441045, HZ451041);
%marcohdat(HZc441046, HZ441046, HZ451042); 
%marcohdat(HZc441047, HZ441047, HZ451043);
%marcohdat(HZc441050, HZ441050, HZ451046);
%marcohdat(HZc441053, HZ441053, HZ451049);
%marcohdat(HZc441054, HZ441054, HZ451050);
%marcohdat(HZc441060, HZ441060, HZ451056);
%marcohdat(HZc441065, HZ441065, HZ451061);
%makeindex(P,supp,T,P,07,HZc441045 HZc441046 HZc441047 HZc441050 HZc441053 HZc441054 HZc441060 HZc441065,7,Part: T Supp-->P 2007);

/*%marcohdat(HZc731026, HZ731026, HZ741023);
%marcohdat(HZc731027, HZ731027, HZ741024); 
%marcohdat(HZc731028, HZ731028, HZ741025);
%marcohdat(HZc731031, HZ731031, HZ741028);
%marcohdat(HZc731034, HZ731034, HZ741031);
%marcohdat(HZc731035, HZ731035, HZ741032);
%marcohdat(HZc731041, HZ731041, HZ741038);
%marcohdat(HZc731046, HZ731046, HZ741043);
%makeindex(P,supp,T,P,10,HZc731026 HZc731027 HZc731028 HZc731031 HZc731034 HZc731035 HZc731041 HZc731046,7,Part: T Supp-->P 2010);*/


/*******************************************************************
   TARGET REPORT OF TARGET'S HOSTILITY TOWARD PARTNER 1995-2010
********************************************************************/

%marcohdat(FTc241065, FT241065, FT251062);
%marcohdat(FTc241067, FT241067, FT251064);
%marcohdat(FTc241068, FT241068, FT251065);
%marcohdat(FTc241073, FT241073, FT251070);
%marcohdat(FTc241072, FT241072, FT251069);
%makeindex(T,host,T,P,95,FTc241065 FTc241067 FTc241068 FTc241073 FTc241072,7,T: T Host-->P 1995); 		/*Created variable: TrThostP95 */

%marcohdat(FTc441075, FT441075, FT451070); 
%marcohdat(FTc441077, FT441077, FT451072); 
%marcohdat(FTc441078, FT441078, FT451073); 
%marcohdat(FTc441083, FT441083, FT451078); 
%marcohdat(FTc441082, FT441082, FT451077); 
%makeindex(T,host,T,P,97,FTc441075 FTc441077 FTc441078 FTc441083 FTc441082,7,T: T Host-->P 1997); 

%marcohdat(FTc641075, FT641075, FT651070); 
%marcohdat(FTc641077, FT641077, FT651072); 
%marcohdat(FTc641078, FT641078, FT651073); 
%marcohdat(FTc641083, FT641083, FT651078); 
%marcohdat(FTc641082, FT641082, FT651077); 
%makeindex(T,host,T,P,99,FTc641075 FTc641077 FTc641078 FTc641083 FTc641082,7,T: T Host-->P 1999); 

%marcohdat(FTc841114, FT841114, FT851109); 
%marcohdat(FTc841118, FT841118, FT851113); 
%marcohdat(FTc841119, FT841119, FT851114); 
%marcohdat(FTc841127, FT841127, FT851122); 
%marcohdat(FTc841129, FT841129, FT851124); 
%makeindex(T,host,T,P,01,FTc841114 FTc841118 FTc841119 FTc841127 FTc841129,7,T: T Host-->P 2001);

%marcohdat(HTc041111, HT041111, HT051110); 
%marcohdat(HTc041115, HT041115, HT051114); 
%marcohdat(HTc041116, HT041116, HT051115); 
%marcohdat(HTc041124, HT041124, HT051123); 
%marcohdat(HTc041126, HT041126, HT051125); 
%makeindex(T,host,T,P,03,HTc041111 HTc041115 HTc041116 HTc041124 HTc041126,7,T: T Host-->P 2003);

%marcohdat(HTc241092, HT241092, HT251091);
%marcohdat(HTc241096, HT241096, HT251095);
%marcohdat(HTc241097, HT241097, HT251096);
%marcohdat(HTc241105, HT241105, HT251104);
%marcohdat(HTc241107, HT241107, HT251106);
%makeindex(T,host,T,P,05,HTc241092 HTc241096 HTc241097 HTc241105 HTc241107,7,T: T Host-->P 2005);

%marcohdat(HTc441095, HT441095, HT451091);
%marcohdat(HTc441099, HT441099, HT451095);
%marcohdat(HTc441100, HT441100, HT451096); 
%marcohdat(HTc441108, HT441108, HT451104); 
%marcohdat(HTc441110, HT441110, HT451106);
%makeindex(T,host,T,P,07,HTc441095 HTc441099 HTc441100 HTc441108 HTc441110,7,T: T Host-->P 2007);

/*%marcohdat(HTc731056, HT731056, HT741053);
%marcohdat(HTc731060, HT731060, HT741057);
%marcohdat(HTc731061, HT731061, HT741058); 
%marcohdat(HTc731069, HT731069, HT741066); 
%marcohdat(HTc731071, HT731071, HT741068);
%makeindex(T,host,T,P,10,HTc731056 HTc731060 HTc731061 HTc731069 HTc731071,7,T: T Host-->P 2010);*/


/*****************************************************************
   TARGET REPORT OF TARGET'S SUPPORT TOWARD PARTNER 1995-2010
******************************************************************/

%marcohdat(FTc241066, FT241066, FT251063);
%marcohdat(FTc241069, FT241069, FT251066);
%marcohdat(FTc241070, FT241070, FT251067);
%marcohdat(FTc241071, FT241071, FT251068);
%makeindex(T,supp,T,P,95,FTc241066 FTc241069 FTc241070 FTc241071,7,T: T Supp-->P 1995);

%marcohdat(FTc441076, FT441076, FT451071); 
%marcohdat(FTc441079, FT441079, FT451074); 
%marcohdat(FTc441080, FT441080, FT451075); 
%marcohdat(FTc441081, FT441081, FT451076);
%makeindex(T,supp,T,P,97,FTc441076 FTc441079 FTc441080 FTc441081,7,T: T Supp-->P 1997);

%marcohdat(FTc641076, FT641076, FT651071); 
%marcohdat(FTc641079, FT641079, FT651074); 
%marcohdat(FTc641080, FT641080, FT651075); 
%marcohdat(FTc641081, FT641081, FT651076);
%makeindex(T,supp,T,P,99,FTc641076 FTc641079 FTc641080 FTc641081,7,T: T Supp-->P 1999);

%marcohdat(FTc841115, FT841115, FT851110);
%marcohdat(FTc841116, FT841116,	FT851111);
%marcohdat(FTc841117, FT841117, FT851112); 
%marcohdat(FTc841120, FT841120, FT851115); 
%marcohdat(FTc841123, FT841123, FT851118); 
%marcohdat(FTc841124, FT841124, FT851119); 
%marcohdat(FTc841130, FT841130,	FT851125);
%marcohdat(FTc841135, FT841135,	FT851130);
%makeindex(T,supp,T,P,01,FTc841117 FTc841120 FTc841123 FTc841124,7,T: T Supp-->P 2001);   /*Based on 4 ITEMS for consistency across waves*/

%marcohdat(HTc041112, HT041112,	HT051111);
%marcohdat(HTc041113, HT041113,	HT051112);
%marcohdat(HTc041114, HT041114, HT051113); 
%marcohdat(HTc041117, HT041117, HT051116); 
%marcohdat(HTc041120, HT041120, HT051119); 
%marcohdat(HTc041121, HT041121, HT051120); 
%marcohdat(HTc041127, HT041127,	HT051126);
%marcohdat(HTc041132, HT041132,	HT051131);
%makeindex(T,supp,T,P,03,HTc041114 HTc041117 HTc041120 HTc041121,7,T: T Supp-->P 2003);   /*Based on 4 ITEMS for consistency across waves*/

%marcohdat(HTc241093, HT241093,	HT251092);
%marcohdat(HTc241094, HT241094,	HT251093);
%marcohdat(HTc241095, HT241095, HT251094);
%marcohdat(HTc241098, HT241098, HT251097);
%marcohdat(HTc241101, HT241101, HT251100);
%marcohdat(HTc241102, HT241102, HT251101); 
%marcohdat(HTc241108, HT241108,	HT251107);
%marcohdat(HTc241113, HT241113,	HT251112);
%makeindex(T,supp,T,P,05,HTc241095 HTc241098 HTc241101 HTc241102,7,T: T Supp-->P 2005);   /*Based on 4 ITEMS for consistency across waves*/

%marcohdat(HTc441096, HT441096,	HT451092);
%marcohdat(HTc441097, HT441097,	HT451093);
%marcohdat(HTc441098, HT441098, HT451094);
%marcohdat(HTc441101, HT441101, HT451097);
%marcohdat(HTc441104, HT441104, HT451100); 
%marcohdat(HTc441105, HT441105, HT451101); 
%marcohdat(HTc441111, HT441111,	HT451107);
%marcohdat(HTc441116, HT441116,	HT451112);
%makeindex(T,supp,T,P,07,HTc441098 HTc441101 HTc441104 HTc441105,7,T: T Supp-->P 2007);  /*Based on 4 ITEMS for consistency across waves*/

/*%marcohdat(HTc731057, HT731057,	HT741054);
%marcohdat(HTc731058, HT731058,	HT741055);
%marcohdat(HTc731059, HT731059, HT741056);
%marcohdat(HTc731062, HT731062, HT741059);
%marcohdat(HTc731065, HT731065, HT741062); 
%marcohdat(HTc731066, HT731066, HT741063); 
%marcohdat(HTc731072, HT731072,	HT741069);
%marcohdat(HTc731077, HT731077,	HT741074);
%makeindex(T,supp,T,P,10,HTc731059 HTc731062 HTc731065 HTc731066,7,T: T Supp-->P 2010);  /*Based on 4 ITEMS for consistency across waves*/


/*******************************************************************
   TARGET REPORT OF PARTNER'S HOSTILITY TOWARD TARGET 1995-2010
********************************************************************/

%marcohdat(FTc241043, FT241043, FT251040); 
%marcohdat(FTc241047, FT241047, FT251044); 
%marcohdat(FTc241048, FT241048, FT251045); 
%marcohdat(FTc241056, FT241056, FT251053); 
%marcohdat(FTc241058, FT241058, FT251055); 
%makeindex(T,host,P,T,95,FTc241043 FTc241047 FTc241048 FTc241056 FTc241058,7,Targ: P Host-->T 1995);

%marcohdat(FTc441045, FT441045, FT451040);
%marcohdat(FTc441049, FT441049, FT451044);
%marcohdat(FTc441050, FT441050, FT451045);
%marcohdat(FTc441058, FT441058, FT451053);
%marcohdat(FTc441060, FT441060, FT451055); 
%makeindex(T,host,P,T,97,FTc441045 FTc441049 FTc441050 FTc441058 FTc441060,7,Targ: P Host-->T 1997);

%marcohdat(FTc641045, FT641045, FT651040); 
%marcohdat(FTc641049, FT641049, FT651044);
%marcohdat(FTc641050, FT641050, FT651045);
%marcohdat(FTc641058, FT641058, FT651053);
%marcohdat(FTc641060, FT641060, FT651055);
%makeindex(T,host,P,T,99,FTc641045 FTc641049 FTc641050 FTc641058 FTc641060,7,Targ: P Host-->T 1999);

%marcohdat(FTc841047, FT841047, FT851042); 
%marcohdat(FTc841051, FT841051, FT851046);
%marcohdat(FTc841052, FT841052, FT851047); 
%marcohdat(FTc841060, FT841060, FT851055); 
%marcohdat(FTc841062, FT841062, FT851057); 
%makeindex(T,host,P,T,01,FTc841047 FTc841051 FTc841052 FTc841060 FTc841062,7,Targ: P Host-->T 2001);

%marcohdat(HTc041044, HT041044, HT051043);
%marcohdat(HTc041048, HT041048, HT051047);
%marcohdat(HTc041049, HT041049, HT051048);
%marcohdat(HTc041057, HT041057, HT051056);
%marcohdat(HTc041059, HT041059, HT051058);
%makeindex(T,host,P,T,03,HTc041044 HTc041048 HTc041049 HTc041057 HTc041059,7,Targ: P Host-->T 2003);

%marcohdat(HTc241041, HT241041, HT251040);
%marcohdat(HTc241045, HT241045, HT251044);
%marcohdat(HTc241046, HT241046, HT251045);
%marcohdat(HTc241054, HT241054, HT251053);
%marcohdat(HTc241056, HT241056, HT251055);
%makeindex(T,host,P,T,05,HTc241041 HTc241045 HTc241046 HTc241054 HTc241056,7,Targ: P Host-->T 2005);

%marcohdat(HTc441044, HT441044, HT451040);
%marcohdat(HTc441048, HT441048, HT451044);
%marcohdat(HTc441049, HT441049, HT451045);
%marcohdat(HTc441057, HT441057, HT451053);
%marcohdat(HTc441059, HT441059, HT451055);
%makeindex(T,host,P,T,07,HTc441044 HTc441048 HTc441049 HTc441057 HTc441059,7,Targ: P Host-->T 2007);

/*%marcohdat(HTc731025, HT731025, HT741022);
%marcohdat(HTc731029, HT731029, HT741026);
%marcohdat(HTc731030, HT731030, HT741027);
%marcohdat(HTc731038, HT731038, HT741035);
%marcohdat(HTc731040, HT731040, HT741037);
%makeindex(T,host,P,T,10,HTc731025 HTc731029 HTc731030 HTc731038 HTc731040,7,Targ: P Host-->T 2010);*/


/*******************************************************************
   TARGET REPORT OF PARTNER'S SUPPORT TOWARD TARGET 1995-2010
********************************************************************/

%marcohdat(FTc241044, FT241044, FT251041); 
%marcohdat(FTc241045, FT241045, FT251042); 
%marcohdat(FTc241046, FT241046, FT251043); 
%marcohdat(FTc241049, FT241049, FT251046); 
%marcohdat(FTc241052, FT241052, FT251049); 
%marcohdat(FTc241053, FT241053, FT251050); 
%marcohdat(FTc241059, FT241059, FT251056); 
%marcohdat(FTc241064, FT241064, FT251061); 
%makeindex(T,supp,P,T,95,FTc241044 FTc241045 FTc241046 FTc241049 FTc241052 FTc241053 FTc241059 FTc241064,7,Targ: P Supp-->T  1995);

%marcohdat(FTc441046, FT441046, FT451041);
%marcohdat(FTc441047, FT441047, FT451042);
%marcohdat(FTc441048, FT441048, FT451043);
%marcohdat(FTc441051, FT441051, FT451046);
%marcohdat(FTc441054, FT441054, FT451049);
%marcohdat(FTc441055, FT441055, FT451050);
%marcohdat(FTc441061, FT441061, FT451056);
%marcohdat(FTc441066, FT441066, FT451061);
%makeindex(T,supp,P,T,97,FTc441046 FTc441047 FTc441048 FTc441051 FTc441054 FTc441055 FTc441061 FTc441066,7,Targ: P Supp-->T  1997);

%marcohdat(FTc641046, FT641046, FT651041);
%marcohdat(FTc641047, FT641047, FT651042);
%marcohdat(FTc641048, FT641048, FT651043);
%marcohdat(FTc641051, FT641051, FT651046);
%marcohdat(FTc641054, FT641054, FT651049);
%marcohdat(FTc641055, FT641055, FT651050);
%marcohdat(FTc641061, FT641061, FT651056);
%marcohdat(FTc641066, FT641066, FT651061);
%makeindex(T,supp,P,T,99,FTc641046 FTc641047 FTc641048 FTc641051 FTc641054 FTc641055 FTc641061 FTc641066,7,Targ: P Supp-->T  1999);

%marcohdat(FTc841048, FT841048, FT851043);
%marcohdat(FTc841049, FT841049, FT851044);
%marcohdat(FTc841050, FT841050, FT851045);
%marcohdat(FTc841053, FT841053, FT851048);
%marcohdat(FTc841056, FT841056, FT851051);
%marcohdat(FTc841057, FT841057, FT851052);
%marcohdat(FTc841063, FT841063, FT851058);
%marcohdat(FTc841068, FT841068, FT851063);
%makeindex(T,supp,P,T,01,FTc841048 FTc841049 FTc841050 FTc841053 FTc841056 FTc841057 FTc841063 FTc841068,7,Targ: P Supp-->T  2001);

%marcohdat(HTc041045, HT041045, HT051044);
%marcohdat(HTc041046, HT041046, HT051045);
%marcohdat(HTc041047, HT041047, HT051046);
%marcohdat(HTc041050, HT041050, HT051049);
%marcohdat(HTc041053, HT041053, HT051052);
%marcohdat(HTc041054, HT041054, HT051053);
%marcohdat(HTc041060, HT041060, HT051059);
%marcohdat(HTc041065, HT041065, HT051064);
%makeindex(T,supp,P,T,03,HTc041045 HTc041046 HTc041047 HTc041050 HTc041053 HTc041054 HTc041060 HTc041065,7,Targ: P Supp-->T  2003);

%marcohdat(HTc241042, HT241042, HT251041);
%marcohdat(HTc241043, HT241043, HT251042); 
%marcohdat(HTc241044, HT241044, HT251043);
%marcohdat(HTc241047, HT241047, HT251046);
%marcohdat(HTc241050, HT241050, HT251049);
%marcohdat(HTc241051, HT241051, HT251050);
%marcohdat(HTc241057, HT241057, HT251056);
%marcohdat(HTc241062, HT241062, HT251061);
%makeindex(T,supp,P,T,05,HTc241042 HTc241043 HTc241044 HTc241047 HTc241050 HTc241051 HTc241057 HTc241062,7,Targ: P Supp-->T  2005);

%marcohdat(HTc441045, HT441045, HT451041);
%marcohdat(HTc441046, HT441046, HT451042); 
%marcohdat(HTc441047, HT441047, HT451043);
%marcohdat(HTc441050, HT441050, HT451046);
%marcohdat(HTc441053, HT441053, HT451049);
%marcohdat(HTc441054, HT441054, HT451050);
%marcohdat(HTc441060, HT441060, HT451056);
%marcohdat(HTc441065, HT441065, HT451061);
%makeindex(T,supp,P,T,07,HTc441045 HTc441046 HTc441047 HTc441050 HTc441053 HTc441054 HTc441060 HTc441065,7,Targ: P Supp-->T  2007);

/*%marcohdat(HTc731026, HT731026, HT741023);
%marcohdat(HTc731027, HT731027, HT741024); 
%marcohdat(HTc731028, HT731028, HT741025);
%marcohdat(HTc731031, HT731031, HT741028);
%marcohdat(HTc731034, HT731034, HT741031);
%marcohdat(HTc731035, HT731035, HT741032);
%marcohdat(HTc731041, HT731041, HT741038);
%marcohdat(HTc731046, HT731046, HT741043);
%makeindex(T,supp,P,T,10,HTc731026 HTc731027 HTc731028 HTc731031 HTc731034 HTc731035 HTc731041 HTc731046,7,Targ: P Supp-->T  2010);*/


/*******************************************************************
   PARTNER REPORT OF PARTNER'S HOSTILITY TOWARD TARGET 1995-2010
********************************************************************/

%marcohdat(FZc241065, FZ241065, FZ251062);
%marcohdat(FZc241067, FZ241067, FZ251064);
%marcohdat(FZc241068, FZ241068, FZ251065);
%marcohdat(FZc241073, FZ241073, FZ251070);
%marcohdat(FZc241072, FZ241072, FZ251069);
%makeindex(P,host,P,T,95,FZc241065 FZc241067 FZc241068 FZc241073 FZc241072,7,Part: P Host-->T  1995); 		/*Created variable: PrPhostT95 */

%marcohdat(FZc441075, FZ441075, FZ451070); 
%marcohdat(FZc441077, FZ441077, FZ451072); 
%marcohdat(FZc441078, FZ441078, FZ451073); 
%marcohdat(FZc441083, FZ441083, FZ451078); 
%marcohdat(FZc441082, FZ441082, FZ451077); 
%makeindex(P,host,P,T,97,FZc441075 FZc441077 FZc441078 FZc441083 FZc441082,7,Part: P Host-->T  1997); 

%marcohdat(FZc641075, FZ641075, FZ651070); 
%marcohdat(FZc641077, FZ641077, FZ651072); 
%marcohdat(FZc641078, FZ641078, FZ651073); 
%marcohdat(FZc641083, FZ641083, FZ651078); 
%marcohdat(FZc641082, FZ641082, FZ651077); 
%makeindex(P,host,P,T,99,FZc641075 FZc641077 FZc641078 FZc641083 FZc641082,7,Part: P Host-->T  1999); 

%marcohdat(FZc841114, FZ841114, FZ851109); 
%marcohdat(FZc841118, FZ841118, FZ851113); 
%marcohdat(FZc841119, FZ841119, FZ851114); 
%marcohdat(FZc841127, FZ841127, FZ851122); 
%marcohdat(FZc841129, FZ841129, FZ851124); 
%makeindex(P,host,P,T,01,FZc841114 FZc841118 FZc841119 FZc841127 FZc841129,7,Part: P Host-->T  2001);

%marcohdat(HZc041111, HZ041111, HZ051110); 
%marcohdat(HZc041115, HZ041115, HZ051114); 
%marcohdat(HZc041116, HZ041116, HZ051115); 
%marcohdat(HZc041124, HZ041124, HZ051123); 
%marcohdat(HZc041126, HZ041126, HZ051125); 
%makeindex(P,host,P,T,03,HZc041111 HZc041115 HZc041116 HZc041124 HZc041126,7,Part: P Host-->T  2003);

%marcohdat(HZc241092, HZ241092, HZ251091);
%marcohdat(HZc241096, HZ241096, HZ251095);
%marcohdat(HZc241097, HZ241097, HZ251096);
%marcohdat(HZc241105, HZ241105, HZ251104);
%marcohdat(HZc241107, HZ241107, HZ251106);
%makeindex(P,host,P,T,05,HZc241092 HZc241096 HZc241097 HZc241105 HZc241107,7,Part: P Host-->T  2005);

%marcohdat(HZc441095, HZ441095, HZ451091);
%marcohdat(HZc441099, HZ441099, HZ451095);
%marcohdat(HZc441100, HZ441100, HZ451096); 
%marcohdat(HZc441108, HZ441108, HZ451104); 
%marcohdat(HZc441110, HZ441110, HZ451106);
%makeindex(P,host,P,T,07,HZc441095 HZc441099 HZc441100 HZc441108 HZc441110,7,Part: P Host-->T  2007);

/*%marcohdat(HZc731056, HZ731056, HZ741053);
%marcohdat(HZc731060, HZ731060, HZ741057);
%marcohdat(HZc731061, HZ731061, HZ741058); 
%marcohdat(HZc731069, HZ731069, HZ741066); 
%marcohdat(HZc731071, HZ731071, HZ741068);
%makeindex(P,host,P,T,10,HZc731056 HZc731060 HZc731061 HZc731069 HZc731071,7,Part: P Host-->T  2010);*/




/******************************************************************
   TARGET REPORT OF TARGET'S RELATIONSHIP INSTABILITY TOWARD PARTNER 1995-2010
********************************************************************/

%marcohdat(FTc241033, FT241033, FT251030);
%marcohdat(FTc241034, FT241034, FT251031);
%marcohdat(FTc241035, FT241035, FT251032);
%marcohdat(FTc241036, FT241036, FT251033);
%marcohdat(FTc241037, FT241037, FT251034);
%makeindex(T,RI,T,P,95,FTc241033 FTc241034 FTc241035 FTc241036 FTc241037,4,Tar: T RelInst-->P  1995);

%marcohdat(FTc441035, FT441035, FT451030); 
%marcohdat(FTc441036, FT441036, FT451031); 
%marcohdat(FTc441037, FT441037, FT451032); 
%marcohdat(FTc441038, FT441038, FT451033);
%marcohdat(FTc441039, FT441039, FT451034);
%makeindex(T,RI,T,P,97,FTc441035 FTc441036 FTc441037 FTc441038 FTc441039,4,Tar: T RelInst-->P  1997);

%marcohdat(FTc641035, FT641035, FT651030); 
%marcohdat(FTc641036, FT641036, FT651031); 
%marcohdat(FTc641037, FT641037, FT651032); 
%marcohdat(FTc641038, FT641038, FT651033);
%marcohdat(FTc641039, FT641039, FT651034);
%makeindex(T,RI,T,P,99,FTc641035 FTc641036 FTc641037 FTc641038 FTc641039,4,Tar: T RelInst-->P  1999);

%marcohdat(FTc841034, FT841034, FT851029);
%marcohdat(FTc841035, FT841035,	FT851030);
%marcohdat(FTc841036, FT841036, FT851031); 
%marcohdat(FTc841037, FT841037, FT851032); 
%marcohdat(FTc841038, FT841038, FT851033); 
%makeindex(T,RI,T,P,01,FTc841034 FTc841035 FTc841036 FTc841037 FTc841038,4,Tar: T RelInst-->P  2001);   

%marcohdat(HTc041030, HT041030,	HT051029);
%marcohdat(HTc041031, HT041031,	HT051030);
%marcohdat(HTc041032, HT041032, HT051031); 
%marcohdat(HTc041033, HT041033, HT051032); 
%marcohdat(HTc041034, HT041034, HT051033); 
%makeindex(T,RI,T,P,03,HTc041030 HTc041031 HTc041032 HTc041033 HTc041034,4,Tar: T RelInst-->P  2003);   

%marcohdat(HTc241027, HT241027,	HT251026);
%marcohdat(HTc241028, HT241028,	HT251027);
%marcohdat(HTc241029, HT241029, HT251028);
%marcohdat(HTc241030, HT241030, HT251029);
%marcohdat(HTc241031, HT241031, HT251030);
%makeindex(T,RI,T,P,05,HTc241027 HTc241028 HTc241029 HTc241030 HTc241031,4,Tar: T RelInst-->P  2005);   

%marcohdat(HTc441030, HT441030,	HT451026);
%marcohdat(HTc441031, HT441031,	HT451027);
%marcohdat(HTc441032, HT441032, HT451028);
%marcohdat(HTc441033, HT441033, HT451029);
%marcohdat(HTc441034, HT441034, HT451030); 
%makeindex(T,RI,T,P,07,HTc441030 HTc441031 HTc441032 HTc441033 HTc441034,4,Tar: T RelInst-->P  2007);  

/******************************************************************
   PARTNER REPORT OF PARTNER'S RELATIONSHIP INSTABILITY TOWARD TARGET 1995-2010
********************************************************************/

%marcohdat(FZc241033, FZ241033, FZ251030);
%marcohdat(FZc241034, FZ241034, FZ251031);
%marcohdat(FZc241035, FZ241035, FZ251032);
%marcohdat(FZc241036, FZ241036, FZ251033);
%marcohdat(FZc241037, FZ241037, FZ251034);
%makeindex(P,RI,P,T,95,FZc241033 FZc241034 FZc241035 FZc241036 FZc241037,4,Part: P RelInst-->T  1995);

%marcohdat(FZc441035, FZ441035, FZ451030); 
%marcohdat(FZc441036, FZ441036, FZ451031); 
%marcohdat(FZc441037, FZ441037, FZ451032); 
%marcohdat(FZc441038, FZ441038, FZ451033);
%marcohdat(FZc441039, FZ441039, FZ451034);
%makeindex(P,RI,P,T,97,FZc441035 FZc441036 FZc441037 FZc441038 FZc441039,4,Part: P RelInst-->T  1997);

%marcohdat(FZc641035, FZ641035, FZ651030); 
%marcohdat(FZc641036, FZ641036, FZ651031); 
%marcohdat(FZc641037, FZ641037, FZ651032); 
%marcohdat(FZc641038, FZ641038, FZ651033);
%marcohdat(FZc641039, FZ641039, FZ651034);
%makeindex(P,RI,P,T,99,FZc641035 FZc641036 FZc641037 FZc641038 FZc641039,4,Part: P RelInst-->T  1999);

%marcohdat(FZc841034, FZ841034, FZ851029);
%marcohdat(FZc841035, FZ841035,	FZ851030);
%marcohdat(FZc841036, FZ841036, FZ851031); 
%marcohdat(FZc841037, FZ841037, FZ851032); 
%marcohdat(FZc841038, FZ841038, FZ851033); 
%makeindex(P,RI,P,T,01,FZc841034 FZc841035 FZc841036 FZc841037 FZc841038,4,Part: P RelInst-->T  2001);   

%marcohdat(HZc041030, HZ041030,	HZ051029);
%marcohdat(HZc041031, HZ041031,	HZ051030);
%marcohdat(HZc041032, HZ041032, HZ051031); 
%marcohdat(HZc041033, HZ041033, HZ051032); 
%marcohdat(HZc041034, HZ041034, HZ051033); 
%makeindex(P,RI,P,T,03,HZc041030 HZc041031 HZc041032 HZc041033 HZc041034,4,Part: P RelInst-->T  2003);   

%marcohdat(HZc241027, HZ241027,	HZ251026);
%marcohdat(HZc241028, HZ241028,	HZ251027);
%marcohdat(HZc241029, HZ241029, HZ251028);
%marcohdat(HZc241030, HZ241030, HZ251029);
%marcohdat(HZc241031, HZ241031, HZ251030);
%makeindex(P,RI,P,T,05,HZc241027 HZc241028 HZc241029 HZc241030 HZc241031,4,Part: P RelInst-->T  2005);   

%marcohdat(HZc441030, HZ441030,	HZ451026);
%marcohdat(HZc441031, HZ441031,	HZ451027);
%marcohdat(HZc441032, HZ441032, HZ451028);
%marcohdat(HZc441033, HZ441033, HZ451029);
%marcohdat(HZc441034, HZ441034, HZ451030); 
%makeindex(P,RI,P,T,07,HZc441030 HZc441031 HZc441032 HZc441033 HZc441034,4,Part: P RelInst-->T  2007);  

run;


data rel;  /**indicates who is partner & trace rel status**/
set indices;

/*COVARIATES*/
*SEX;
format gender sex.; /**labels sex**/

*SAMPLE (IYFP/ISPP);
format origsamp samp.; 

*PARTNER TYPE IN 95, 97, 99, 01, 03, 05, 07; 
if HT731001=1 then relstat10=1;  /**from Q3**/
else if HT731001=2 then relstat10=2; 
else if 1<=HT741001<=4 then relstat10=3; /**from Q4**/
else if f17restat=1 then relstat10=1;  /**from participation file**/
else if f17restat in (2,3) then relstat10=2; 
else if f17restat in (4,5) then relstat10=3; 
else if f17restat = 6 then relstat10=4;
else if f17restat = 7 then relstat10=5;
format relstat10 relstat.; 

if HT441001=1 then relstat07=1;  /**from Q4**/
else if HT441001=2 then relstat07=2; 
else if 1<=HT451001<=4 then relstat07=3; /**from Q5**/
else if f14restat=1 then relstat07=1;  /**from participation file**/
else if f14restat in (2,3) then relstat07=2; 
else if f14restat in (4,5) then relstat07=3; 
else if f14restat = 6 then relstat07=4;
else if f14restat = 7 then relstat07=5;
format relstat07 relstat.; 

if HT241001=1 then relstat05=1; 
else if HT241001=2 then relstat05=2; 
else if 1<=HT251001<=4 then relstat05=3; 
else if f12restat=1 then relstat05=1; 
else if f12restat in (2,3) then relstat05=2; 
else if f12restat in (4,5) then relstat05=3; 
else if f12restat = 6 then relstat05=4;
else if f12restat = 7 then relstat05=5;
format relstat05 relstat.; 

if HT041001=1 then relstat03=1; 
else if HT041001=2 then relstat03=2; 
else if 1<=HT051001<=4 then relstat03=3; 
else if f10restat=1 then relstat03=1; 
else if f10restat in (2,3) then relstat03=2; 
else if f10restat in (4,5) then relstat03=3; 
else if f10restat = 6 then relstat03=4;
else if f10restat = 7 then relstat03=5;
format relstat03 relstat.; 

if FT841001=1 then relstat01=1; 
else if FT841001=2 then relstat01=2; 
else if 1<=FT851001<=4 then relstat01=3; 
else if f8restat=1 then relstat01=1; 
else if f8restat in (2,3) then relstat01=2; 
else if f8restat in (4,5) then relstat01=3; 
else if f8restat = 6 then relstat01=4;
else if f8restat = 7 then relstat01=5;
format relstat01 relstat.;

if FT641001=1 then relstat99=1;
else if FT641001=2 then relstat99=2; 
else if 1<=FT651001<=4 then relstat99=3; 
else if f6restat=1 then relstat99=1; 
else if f6restat in (2,3) then relstat99=2; 
else if f6restat in (4,5) then relstat99=3; 
else if f6restat=6 then relstat99=4; 
else if f6restat=7 then relstat99=5;
format relstat99 relstat.; 

if FT441001=1 then relstat97=1; 
else if FT441001=2 then relstat97=2; 
else if 1<=FT451001<=4 then relstat97=3; 
else if f4restat=1 then relstat97=1; 
else if f4restat in (2,3) then relstat97=2; 
else if f4restat in (4,5) then relstat97=3; 
else if f4restat=6 then relstat97=4; 
else if f4restat=7 then relstat97=5; 
format relstat97 relstat.; 

if FT241001=1 then relstat95=1; 
else if FT241001=2 then relstat95=2; 
else if 1<=FT251002<=4 then relstat95=3; 
else if f2restat=1 then relstat95=1; 
else if f2restat in (2,3) then relstat95=2; 
else if f2restat in (4,5) then relstat95=3; 
else if f2restat=6 then relstat95=4; 
else if f2restat=7 then relstat95=5; 
format relstat95 relstat.; 

run;


data rel2; 
set rel; 

/******INTERVIEW DATES  --> Use variables from participation file tf2int - tf14int ******/
array intdate (7) tf2int tf4int tf6int tf8int tf10int tf12int tf14int; /**intdate=interview date; (7) = 7 elements**/
format tf2int tf4int tf6int tf8int tf10int tf12int tf14int mmddyys10.;


/******DATES OF MARRIAGE AND COHABITING REPORTED IN RELATIONSHIP QUESTIONNAIRES******/
array dates(*) 
ht441002 ht441003 ht441008 ht441009
ht241002 ht241003 ht241005 ht241006
ht041002 ht041003 ht041005 ht041006  
ft841004 ft841005 ft841009 ft841010
ft641002 ft641003 ft641007 ft641008 
ft441002 ft441003 ft441007 ft441008 
ft241002 ft241003 ft241006 ft241007; 
do i=1 to 28; /**1 to 28 = do for all 28 variables**/
if dates(i) in (88,99,8888,9999) then dates(i)=.; /**specifying missing values: .=missing value**/
end; 
array threshdate(7) d2 d4 d6 d8 d10 d12 d14; 
array datetype(7) t2 t4 t6 t8 t10 t12 t14; /**married/cohabiting**/
%macro threshdates(round,marmo,maryr,como,coyr); 
if &marmo ne . and &maryr ne . then do; 
	d&round=mdy(&marmo,15,&maryr); 
	t&round=1; 
end; 
else if &como ne . and &coyr ne . then do; 
	d&round=mdy(&como,15,&coyr); 
	t&round=2; 
end; 
%mend; 
%threshdates(2,ft241002,ft241003,ft241006,ft241007); 
%threshdates(4,ft441002,ft441003,ft441007,ft441008); 
%threshdates(6,ft641002,ft641003,ft641007,ft641008); 
%threshdates(8,ft841004,ft841005,ft841009,ft841010); 
%threshdates(10,ht041002,ht041003,ht041005,ht041006); 
%threshdates(12,ht241002,ht241003,ht241005,ht241006); 
%threshdates(14,ht441002,ht441003,ht441008,ht441009);
format d2 d4 d6 d8 d10 d12 d14 mmddyys10.;
format t2 t4 t6 t8 t10 t12 t14 datetype.; 

/*proc freq; 
tables relstat95*d2*t2*ft241002*ft241003*ft241006*ft241007
relstat97*d4*t4*ft441002*ft441003*ft441007*ft441008
relstat99*d6*t6*ft641002*ft641003*ft641007*ft641008
relstat01*d8*t8*ft841004*ft841005*ft841009*ft841010
relstat03*d10*t10*ht041002*ht041003*ht041005*ht041006
relstat05*d12*t12*ht241002*ht241003*ht241005*ht241006
relstat07*d14*t14*ht441002*ht441003*ht441008*ht441009 /list missing; 
run;*/


/******FIRST MARITAL OR COHABITING RELATIONSHIP******/
array opids(7) f2opidn f4opidn f6opidn f8opidn f10opidn f12opidn f14opidn; 
format f4opbday f6opbday f8opbday f10opbday f12opbday f14opbday mmddyys10.;
array opbday(7)null f4opbday f6opbday f8opbday f10opbday f12opbday f14opbday;
if nmiss(of threshdate(*))=7 then do; 
		bigday=.N; /**date of first mar/coh rel reported**/
		bigtype=.N; /**married=1 or cohabit=2**/
		bigopid=.N; /**id number of partner (1st partner)**/
		bigopbday=.N; end; /**.N = no OP**/
else if nmiss(of threshdate(*))<7 then do; 
	round=0; 
	do until (bigday ne .); 
		round=round+1; 
		bigday=threshdate(round);
		bigtype=datetype(round);  
		bigopid=opids(round);
		bigopbday=opbday(round);  
		end; 
end; 
round=round*2;  /**checks how many marriages reported across waves**/
if FT835001=1 then nummar8=0; 
	else if FT835001=2 then nummar8=1; 
	else if FT835001=3 then nummar8=2; 
	else if FT835001=4 then nummar8=3; 
	else if FT835001=5 then nummar8=4; 
	else if FT835001 in (8,9) then nummar8=.; 
if HT035001=1 then nummar10=0; 
	else if ht035001=2 then nummar10=1; 
	else if ht035001=3 then nummar10=2; 
	else if ht035001=4 then nummar10=3; 
	else if ht035001=5 then nummar10=4; 
	else if ht035001 in (8,9) then nummar10=.; 
if ht2mar=1 then nummar12=1; 
	else if ht2mar=2 then nummar12=2; 
	else if ht2mar=3 then nummar12=3; 
	else if ht2mar=4 then nummar12=4; 
	else if ht2mar=5 then nummar12=0; 
	else if ht2mar in (8,9) then nummar12=.; 
if ht4mar=1 then nummar12=1; 
	else if ht4mar=2 then nummar14=2; 
	else if ht4mar=3 then nummar14=3; 
	else if ht4mar=4 then nummar14=4; 
	else if ht4mar=5 then nummar14=0; 
	else if ht4mar in (8,9) then nummar14=.; 
if round=8 then bignummar=nummar8; 
else if round=10 then bignummar=nummar10; 
else if round=12 then bignummar=nummar12;
else if round=14 then bignummar=nummar14; 

format bigday bigopbday mmddyys10.;
format bigtype datetype.; 

/*proc print data=rel2(obs=10); 
var bigday bigtype bigopid round d2 d4 d6 d8 d10 d12 d14 t2 t4 t6 t8 t10 t12 t14 f2opidn f4opidn f6opidn f8opidn f10opidn f12opidn f14opidn; 
run;
proc print data=rel2(obs=10); 
var bigday bigtype bigopid bigopbday round d2 d4 d6 d8 d10 d12 d14 t2 t4 t6 t8 t10 t12 t14 f4opbday f6opbday f8opbday f10opbday f12opbday f14opbday; 
run;*/


/******ARE OP'S IN OTHER WAVES THE SAME AS THE ONE IN THE FIRST MARITAL OR COHABITING RELATIONSHIP?******/
array bigop(7) bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14; 
if bigopid=.N then do; 
	do i=1 to 7; 
	bigop(i)=.N; end;
end;
else if bigopid ne .N then do; 
	do i=1 to 7; 
	if opids(i)=bigopid then bigop(i)=1; /*yes, it's the same person*/
	else if opids(i) ne bigopid then bigop(i)=0; /*no, not the first marital or cohabiting op*/
	if opids(i)=. then bigop(i)=.; 
end; end;
do i=1 to 7; 
	if bigop(i)=. then do; 
		if (opbday(i)=.) or (bigopbday=.) then bigop(i)=.; 
		else if opbday(i)=bigopbday then bigop(i)=1; 
		else if opbday(i) ne bigopbday then bigop(i)=0; 
	end; 
end;	


/*OP BIRTHDAYS FOR F5, F7, F9, F11, F13, F15 --> Use variables from participation file: f5opbday - f15opbday*/
format f5opbday f7opbday f9opbday f11opbday f13opbday f15opbday mmddyys10.; 



/*******TIME (IN MONTHS) OF EACH SURVEY ROUND RELATIVE TO "BIGDAY" (DATE OF FIRST MARRIAGE/COHABITATION)******/
reldate2=round(((tf2int-bigday)/365.25)*12);  /**compares date married to date interviewed**/
reldate4=round(((tf4int-bigday)/365.25)*12); 
reldate6=round(((tf6int-bigday)/365.25)*12); 
reldate8=round(((tf8int-bigday)/365.25)*12); 
reldate10=round(((tf10int-bigday)/365.25)*12); 
reldate12=round(((tf12int-bigday)/365.25)*12); 
reldate14=round(((tf14int-bigday)/365.25)*12); 
array reldate(*) reldate2 reldate4 reldate6 reldate8 reldate10 reldate12 reldate14; 
do i=1 to 7; 
	if bigday=.N then reldate(i)=.N; 
end; 
/*proc print data=rel2(obs=120); 
var bigday tf2int reldate2 tf4int reldate4 tf6int reldate6 tf8int reldate8 tf10int reldate10 tf12int reldate12 tf14int reldate14; 
run;*/


/****** BIGROUND =  WHICH ROUND IS CLOSEST FOLLOWING THE "BIGDAY"? (DATE OF FIRST MARRIAGE/COHABITATION) ******/
/** Choose the first measurement occasion following BIGDAY (i.e., smallest positive value after BIGDAY) **/
if reldate2=reldate4=reldate6=reldate8=reldate10=reldate12=reldate14=.N then biground=.N;
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate2) then do; 
	if reldate2>=0 then biground=2; else biground=4; end;  
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate4) then do; 
	if reldate4>=0 then biground=4; else biground=6; end; 
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate6) then do; 
	if reldate6>=0 then biground=6; else biground=8; end; 
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate8) then do; 
	if reldate8>=0 then biground=8; else biground=10; end; 
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate10) then do; 
	if reldate10>=0 then biground=10; else biground=12; end; 
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate12) then do;
	if reldate12>=0 then biground=12; else biground=14; end;
else if min(abs(reldate2),abs(reldate4),abs(reldate6),abs(reldate8),abs(reldate10),abs(reldate12),abs(reldate14))= abs(reldate14) then biground=14;
/*proc print data=rel2(obs=20); 
var biground reldate2 reldate4 reldate6 reldate8 reldate10 reldate12 reldate14; 
run;
proc print data=rel2(obs=20); 
var biground bigday tf2int tf4int tf6int tf8int tf10int tf12int tf14int;
run;*/ 


/******ARE OP'S IN OTHER WAVES THE SAME AS THE ONE IN THE FIRST MARITAL OR COHABITING RELATIONSHIP? (CONT.) ******/
do i=1 to 7;
	if bigop(i)=. and biground=i*2 then bigop(i)=1; 
end; 
array rels(7) relstat95 relstat97 relstat99 relstat01 relstat03 relstat05 relstat07; 
do i=1 to 7; 
	if bigop(i)=. and rels(i)=4 then bigop(i)=0; 
    if bigop(i)=. and rels(i)=5 then bigop(i)=0;
end; 
if 2<=biground<14 then do; 
do i=((biground/2)+1) to 7; 
	if bigop(i)=. and rels(i)=3 then bigop(i)=0; 
end; end; 
if (bigop2=1) and (bigop4=.) then do; 
	if bigopid=f5opidn then bigop4=1; 
	if bigopbday=f5opbday then bigop4=1; end; 
if (bigop4=1) and (bigop6=.) then do; 
	if bigopid=f7opidn then bigop6=1; 
	if bigopbday=f7opbday then bigop6=1; end; 
if (bigop6=1) and (bigop8=.) then do; 
	if bigopid=f9opidn then bigop8=1; 
	if bigopbday=f9opbday then bigop8=1; end; 
if (bigop8=1) and (bigop10=.) then do; 
	if bigopid=f11opidn then bigop10=1; 
	if bigopbday=f11opbday then bigop10=1; end; 
if (bigop10=1) and (bigop12=.) then do; 
	if bigopid=f13opidn then bigop12=1; 
	if bigopbday=f13opbday then bigop12=1; end; 
if (bigop12=1) and (bigop14=.) then do; 
	if bigopid=f15opidn then bigop14=1; 
	if bigopbday=f15opbday then bigop14=1; end; 

/*proc print data=rel2(obs=20); 
var bigday bigtype bigopid bigopbday round bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14 
f4opbday f6opbday f8opbday f10opbday f12opbday f14opbday; 
run;*/

if (bigop2=0) and (bigop4=.) and (bigop6=0) then bigop4=0; 
if (bigop4=0) and (bigop6=.) and (bigop8=0) then bigop6=0; 
if (bigop6=0) and (bigop8=.) and (bigop10=0) then bigop8=0; 
if (bigop8=0) and (bigop10=.) and (bigop12=0) then bigop10=0; 
if (bigop10=0) and (bigop12=.) and (bigop14=0) then bigop12=0; 

if bigop2=1 and bigop4=. and bigop6=1 and relstat95=relstat99 then bigop4=1; 
if bigop4=1 and bigop6=. and bigop8=1 and relstat97=relstat01 then bigop6=1; 
if bigop6=1 and bigop8=. and bigop10=1 and relstat99=relstat03 then bigop8=1; 
if bigop8=1 and bigop10=. and bigop12=1 and relstat01=relstat05 then bigop10=1; 
if bigop10=1 and bigop12=. and bigop14=1 and relstat03=relstat07 then bigop12=1; 


/*Based on Divorce records --> Future round(s) following divorce date will be set to zero (or missing, if target died) */
if famid=2 then do; 
	bigop14=0; end; 
if famid=7 then do; 
	bigop12=0; bigop14=0; end;
if famid=16 then do;    /*Famid 16 died Oct 2001*/
	bigop10=.; bigop12=.; bigop14=.; end; 
if famid=29 then do; 
	bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end; 
if famid=50 then do; 
	bigop12=0; bigop14=0; end; 

if famid=62 then do;
	bigop4=0; bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=70 then do;
	bigop12=0; bigop14=0; end;
if famid=89 then do; 
	bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end; 
if famid=109 then bigop14=0; 
if famid=111 then do;
	bigop12=0; bigop14=0; end; 

if famid=117 then do;
	bigop4=0; bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=147 then do; 
	bigop12=0; bigop14=0; end;
if famid=160 then do;
	bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=170 then bigop14=0; 
if famid=173 then do;
	bigop10=0; bigop12=0; bigop14=0; end;

if famid=175 then do;
	bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=177 then do; 
	bigop8=1; bigop10=0; bigop12=0; bigop14=0; end;
if famid=196 then do;
	bigop10=0; bigop12=0; bigop14=0; end;
if famid=215 then do;
	bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=230 then do;
	bigop10=0; bigop12=0; bigop14=0; end;

if famid=232 then do;
	bigop10=0; bigop12=0; bigop14=0; end;
if famid=282 then do;
	bigop12=0; bigop14=0; end;
if famid=297 then do;
	bigop10=0; bigop12=0; bigop14=0; end;
if famid=308 then do;
	bigop12=0; bigop14=0; end;
if famid=309 then do;
	bigop12=0; bigop14=0; end;

if famid=319 then do;
	bigop14=0; end;
if famid=330 then do; 
	bigop10=1; bigop12=1; bigop14=0; end;
if famid=346 then do;
	bigop12=0; bigop14=0; end;
if famid=383 then do;
	bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=392 then do;
	bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;

if famid=401 then do;
	bigop6=0; bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=410 then do;
	bigop12=0; bigop14=0; end;
if famid=430 then do;
	bigop14=0; end;
if famid=435 then  do;
	bigop8=0; bigop10=0; bigop12=0; bigop14=0; end;
if famid=436 then do; 
	bigop8=1; bigop10=1; bigop12=0; bigop14=0; end;

if famid=441 then do;
	bigop10=0; bigop12=0; bigop14=0; end;
if famid=513 then do;
	bigop10=0; bigop12=0; bigop14=0; end;
if famid=523 then do;
	bigop12=0; bigop14=0; end;
if famid=595 then do;
	bigop12=0; bigop14=0; end;
if famid=706 then do;
 	bigop10=0; bigop12=0; bigop14=0; end;

/*More Specific Codings*/
if famid=391 then do; 
	bigop8=1; bigop10=1; bigop12=1; end;
if famid=387 then do; 
	bigop8=1; bigop10=1; bigop12=1; end;
if famid=323 then do; 
	bigop8=1; bigop10=1; bigop12=1; end;
if famid=453 then bigop4=1;
if famid=454 then do; 
	bigop8=1; bigop10=1; bigop12=1;  end;
if famid=509 then do; 
	bigop10=1; bigop12=1; end;
if famid=566 then bigop10=0; 
if famid=694 then do; 
	bigop8=0; bigop10=0; end;
if famid=262 then do; 
	bigop8=1; bigop10=1; bigop12=1; end;
if famid=246 then bigop10=1; 
if famid=188 then do; 
	bigop8=0; bigop10=0; end;
if famid=137 then do;
 	bigop8=1; bigop10=1; end; 

/*proc print data=rel2(obs=20); 
var bigday bigtype bigopid bigopbday round bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14 
relstat95 relstat97 relstat99 relstat01 relstat03 relstat05 relstat07;
run;*/

run;



data rel3;
set rel2;

array bigop(7) bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14; 

/******"RELTIME" MACRO: CONVERTS AN ARRAY OF LENGTH 7 VARIABLES 
(F2,F4,F6,F8,F10,F12,F14,F17)INTO TIME RELATIVE TO THE DATE OF FIRST MARRIAGE OR COHAB******/
%macro reltime(root,vars8); 
array &root(7) &vars8; /*7 variables on original time scale*/
array &root._reltime(13) &root.12B_ &root.10B_ &root.8B_ &root.6B_ &root.4B_ &root.2B_ &root.0_ &root.2A_ &root.4A_ &root.6A_ &root.8A_ &root.10A_ &root.12A_; /*relative to first marriage/cohab*/
array &root._reltime2(13) &root.12B &root.10B &root.8B &root.6B &root.4B &root.2B &root.0 &root.2A &root.4A &root.6A &root.8A &root.10A &root.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
/*for all partners, including first marital or cohabiting partner*/
if biground=.N then do; 
	&root._reltime(1)=.N;
	&root._reltime(2)=.N; 
	&root._reltime(3)=.N;
	&root._reltime(4)=.N;
	&root._reltime(5)=.N;
	&root._reltime(6)=.N;
	&root._reltime(7)=.N; 
	&root._reltime(8)=.N;
	&root._reltime(9)=.N;
	&root._reltime(10)=.N;
	&root._reltime(11)=.N;
	&root._reltime(12)=.N;
	&root._reltime(13)=.N;end;   
if biground=2 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=.A; 
	&root._reltime(3)=.A;
	&root._reltime(4)=.A;
	&root._reltime(5)=.A;
	&root._reltime(6)=.A;
	&root._reltime(7)=&root(1); 
	&root._reltime(8)=&root(2); 
	&root._reltime(9)=&root(3); 
	&root._reltime(10)=&root(4); 
	&root._reltime(11)=&root(5); 
	&root._reltime(12)=&root(6); 
	&root._reltime(13)=&root(7); end; 
else if biground=4 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=.A; 
	&root._reltime(3)=.A;
	&root._reltime(4)=.A;
	&root._reltime(5)=.A;
	&root._reltime(6)=&root(1); 
	&root._reltime(7)=&root(2); 
	&root._reltime(8)=&root(3); 
	&root._reltime(9)=&root(4); 
	&root._reltime(10)=&root(5);
 	&root._reltime(11)=&root(6);
	&root._reltime(12)=&root(7);
	&root._reltime(13)=.B; end;
else if biground=6 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=.A; 
	&root._reltime(3)=.A;
	&root._reltime(4)=.A;
	&root._reltime(5)=&root(1);
	&root._reltime(6)=&root(2); 
	&root._reltime(7)=&root(3); 
	&root._reltime(8)=&root(4); 
	&root._reltime(9)=&root(5); 
	&root._reltime(10)=&root(6);
	&root._reltime(11)=&root(7);
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end; 
else if biground=8 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=.A; 
	&root._reltime(3)=.A; 
	&root._reltime(4)=&root(1); 
	&root._reltime(5)=&root(2);	
	&root._reltime(6)=&root(3); 
	&root._reltime(7)=&root(4); 
	&root._reltime(8)=&root(5); 
	&root._reltime(9)=&root(6); 
	&root._reltime(10)=&root(7); 
	&root._reltime(11)=.B;
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end;
else if biground=10 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=.A; 
	&root._reltime(3)=&root(1);
	&root._reltime(4)=&root(2);
	&root._reltime(5)=&root(3);
	&root._reltime(6)=&root(4); 
	&root._reltime(7)=&root(5); 
	&root._reltime(8)=&root(6); 
	&root._reltime(9)=&root(7); 
	&root._reltime(10)=.B; 
	&root._reltime(11)=.B; 
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end; 
else if biground=12 then do; 
	&root._reltime(1)=.A;
	&root._reltime(2)=&root(1);  
	&root._reltime(3)=&root(2);
	&root._reltime(4)=&root(3);
	&root._reltime(5)=&root(4);
	&root._reltime(6)=&root(5); 
	&root._reltime(7)=&root(6); 
	&root._reltime(8)=&root(7); 
	&root._reltime(9)=.B;
	&root._reltime(10)=.B; 
	&root._reltime(11)=.B; 
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end; 
else if biground=14 then do; 
	&root._reltime(1)=&root(1);
	&root._reltime(2)=&root(2); 
	&root._reltime(3)=&root(3);
	&root._reltime(4)=&root(4);
	&root._reltime(5)=&root(5);
	&root._reltime(6)=&root(6); 
	&root._reltime(7)=&root(7); 
	&root._reltime(8)=.B; 
	&root._reltime(9)=.B; 
	&root._reltime(10)=.B; 
	&root._reltime(11)=.B; 
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end; 
	else if biground=17 then do; 
	&root._reltime(1)=&root(1);
	&root._reltime(2)=&root(2); 
	&root._reltime(3)=&root(3);
	&root._reltime(4)=&root(4);
	&root._reltime(5)=&root(5);
	&root._reltime(6)=&root(6); 
	&root._reltime(7)=&root(7); 
	&root._reltime(8)=.B; 
	&root._reltime(9)=.B; 
	&root._reltime(10)=.B; 
	&root._reltime(11)=.B; 
	&root._reltime(12)=.B;
	&root._reltime(13)=.B; end; 
/*only for the 1st marital or cohabiting partner*/ 
if biground=.N then do; 
	&root._reltime2(1)=.N;
	&root._reltime2(2)=.N; 
	&root._reltime2(3)=.N;
	&root._reltime2(4)=.N;
	&root._reltime2(5)=.N;
	&root._reltime2(6)=.N;
	&root._reltime2(7)=.N; 
	&root._reltime2(8)=.N;
	&root._reltime2(9)=.N;
	&root._reltime2(10)=.N;
	&root._reltime2(11)=.N; 
	&root._reltime2(12)=.N;
	&root._reltime2(13)=.N; end;
 else if biground=2 then do; 
	&root._reltime2(1)=.A;
	&root._reltime2(2)=.A; 
	&root._reltime2(3)=.A;
	&root._reltime2(4)=.A;
	&root._reltime2(5)=.A;
	&root._reltime2(6)=.A;
	if bigop(1)=1 then &root._reltime2(7)=&root(1); else if bigop(1)=0 then &root._reltime2(7)=.O; else if bigop(1)=. then &root._reltime2(7)=.U; 
	if bigop(2)=1 then &root._reltime2(8)=&root(2); else if bigop(2)=0 then &root._reltime2(8)=.O;  else if bigop(2)=. then &root._reltime2(8)=.U;
	if bigop(3)=1 then &root._reltime2(9)=&root(3); else if bigop(3)=0 then &root._reltime2(9)=.O; else if bigop(3)=. then &root._reltime2(9)=.U; 
	if bigop(4)=1 then &root._reltime2(10)=&root(4); else if bigop(4)=0 then &root._reltime2(10)=.O; else if bigop(4)=. then &root._reltime2(10)=.U;
	if bigop(5)=1 then &root._reltime2(11)=&root(5); else if bigop(5)=0 then &root._reltime2(11)=.O;  else if bigop(5)=. then &root._reltime2(11)=.U; 
	if bigop(6)=1 then &root._reltime2(12)=&root(6); else if bigop(6)=0 then &root._reltime2(12)=.O; else if bigop(6)=. then &root._reltime2(12)=.U; 
	if bigop(7)=1 then &root._reltime2(13)=&root(7); else if bigop(7)=0 then &root._reltime2(13)=.O; else if bigop(7)=. then &root._reltime2(13)=.U; end; 
else if biground=4 then do; 
	&root._reltime2(1)=.A;
	&root._reltime2(2)=.A; 
	&root._reltime2(3)=.A;
	&root._reltime2(4)=.A;
	&root._reltime2(5)=.A;
	if bigop(1)=1 then &root._reltime2(6)=&root(1); else if bigop(1)=0 then &root._reltime2(6)=.O; else if bigop(1)=. then &root._reltime2(6)=.U; 
	if bigop(2)=1 then &root._reltime2(7)=&root(2); else if bigop(2)=0 then &root._reltime2(7)=.O; else if bigop(2)=. then &root._reltime2(7)=.U; 
	if bigop(3)=1 then &root._reltime2(8)=&root(3); else if bigop(3)=0 then &root._reltime2(8)=.O; else if bigop(3)=. then &root._reltime2(8)=.U; 
	if bigop(4)=1 then &root._reltime2(9)=&root(4); else if bigop(4)=0 then &root._reltime2(9)=.O; else if bigop(4)=. then &root._reltime2(9)=.U; 
	if bigop(5)=1 then &root._reltime2(10)=&root(5); else if bigop(5)=0 then &root._reltime2(10)=.O; else if bigop(5)=. then &root._reltime2(10)=.U; 
	if bigop(6)=1 then &root._reltime2(11)=&root(6); else if bigop(6)=0 then &root._reltime2(11)=.O; else if bigop(6)=. then &root._reltime2(11)=.U; 
	if bigop(7)=1 then &root._reltime2(12)=&root(7); else if bigop(7)=0 then &root._reltime2(12)=.O; else if bigop(7)=. then &root._reltime2(12)=.U; 
	&root._reltime2(13)=.B; end;
else if biground=6 then do; 
	&root._reltime2(1)=.A;
	&root._reltime2(2)=.A; 
	&root._reltime2(3)=.A;
	&root._reltime2(4)=.A;
	if bigop(1)=1 then &root._reltime2(5)=&root(1); else if bigop(1)=0 then &root._reltime2(5)=.O; else if bigop(1)=. then &root._reltime2(5)=.U; 
	if bigop(2)=1 then &root._reltime2(6)=&root(2); else if bigop(2)=0 then &root._reltime2(6)=.O; else if bigop(2)=. then &root._reltime2(6)=.U; 
	if bigop(3)=1 then &root._reltime2(7)=&root(3); else if bigop(3)=0 then &root._reltime2(7)=.O; else if bigop(3)=. then &root._reltime2(7)=.U; 
	if bigop(4)=1 then &root._reltime2(8)=&root(4); else if bigop(4)=0 then &root._reltime2(8)=.O; else if bigop(4)=. then &root._reltime2(8)=.U; 
	if bigop(5)=1 then &root._reltime2(9)=&root(5); else if bigop(5)=0 then &root._reltime2(9)=.O; else if bigop(5)=. then &root._reltime2(9)=.U; 
	if bigop(6)=1 then &root._reltime2(10)=&root(6); else if bigop(6)=0 then &root._reltime2(10)=.O; else if bigop(6)=. then &root._reltime2(10)=.U; 
	if bigop(7)=1 then &root._reltime2(11)=&root(7); else if bigop(7)=0 then &root._reltime2(11)=.O; else if bigop(7)=. then &root._reltime2(11)=.U; 
	&root._reltime2(12)=.B;
	&root._reltime2(13)=.B; end; 
else if biground=8 then do; 
	&root._reltime2(1)=.A;
	&root._reltime2(2)=.A; 
	&root._reltime2(3)=.A; 
	if bigop(1)=1 then &root._reltime2(4)=&root(1); else if bigop(1)=0 then &root._reltime2(4)=.O; else if bigop(1)=. then &root._reltime2(4)=.U; 
	if bigop(2)=1 then &root._reltime2(5)=&root(2);	else if bigop(2)=0 then &root._reltime2(5)=.O; else if bigop(2)=. then &root._reltime2(5)=.U; 
	if bigop(3)=1 then &root._reltime2(6)=&root(3); else if bigop(3)=0 then &root._reltime2(6)=.O; else if bigop(3)=. then &root._reltime2(6)=.U; 
	if bigop(4)=1 then &root._reltime2(7)=&root(4); else if bigop(4)=0 then &root._reltime2(7)=.O; else if bigop(4)=. then &root._reltime2(7)=.U; 
	if bigop(5)=1 then &root._reltime2(8)=&root(5); else if bigop(5)=0 then &root._reltime2(8)=.O; else if bigop(5)=. then &root._reltime2(8)=.U; 
	if bigop(6)=1 then &root._reltime2(9)=&root(6); else if bigop(6)=0 then &root._reltime2(9)=.O; else if bigop(6)=. then &root._reltime2(9)=.U; 
	if bigop(7)=1 then &root._reltime2(10)=&root(7); else if bigop(7)=0 then &root._reltime2(10)=.O; else if bigop(7)=. then &root._reltime2(10)=.U;  
	&root._reltime2(11)=.B;
	&root._reltime2(12)=.B;
	&root._reltime2(13)=.B; end;
else if biground=10 then do; 
	&root._reltime2(1)=.A;
	&root._reltime2(2)=.A; 
	if bigop(1)=1 then &root._reltime2(3)=&root(1); else if bigop(1)=0 then &root._reltime2(3)=.O; else if bigop(1)=. then &root._reltime2(3)=.U; 
	if bigop(2)=1 then &root._reltime2(4)=&root(2); else if bigop(2)=0 then &root._reltime2(4)=.O; else if bigop(2)=. then &root._reltime2(4)=.U; 
	if bigop(3)=1 then &root._reltime2(5)=&root(3); else if bigop(3)=0 then &root._reltime2(5)=.O; else if bigop(3)=. then &root._reltime2(5)=.U; 
	if bigop(4)=1 then &root._reltime2(6)=&root(4); else if bigop(4)=0 then &root._reltime2(6)=.O; else if bigop(4)=. then &root._reltime2(6)=.U; 
	if bigop(5)=1 then &root._reltime2(7)=&root(5); else if bigop(5)=0 then &root._reltime2(7)=.O; else if bigop(5)=. then &root._reltime2(7)=.U; 
	if bigop(6)=1 then &root._reltime2(8)=&root(6); else if bigop(6)=0 then &root._reltime2(8)=.O; else if bigop(6)=. then &root._reltime2(8)=.U; 
	if bigop(7)=1 then &root._reltime2(9)=&root(7); else if bigop(7)=0 then &root._reltime2(9)=.O; else if bigop(7)=. then &root._reltime2(9)=.U; 
	&root._reltime2(10)=.B; 
	&root._reltime2(11)=.B; 
	&root._reltime2(12)=.B;
	&root._reltime2(13)=.B; end; 
else if biground=12 then do; 
	&root._reltime2(1)=.A;
	if bigop(1)=1 then &root._reltime2(2)=&root(1); else if bigop(1)=0 then &root._reltime2(2)=.O; else if bigop(1)=. then &root._reltime2(2)=.U; 
	if bigop(2)=1 then &root._reltime2(3)=&root(2); else if bigop(2)=0 then &root._reltime2(3)=.O; else if bigop(2)=. then &root._reltime2(3)=.U; 
	if bigop(3)=1 then &root._reltime2(4)=&root(3); else if bigop(3)=0 then &root._reltime2(4)=.O; else if bigop(3)=. then &root._reltime2(4)=.U; 
	if bigop(4)=1 then &root._reltime2(5)=&root(4); else if bigop(4)=0 then &root._reltime2(5)=.O; else if bigop(4)=. then &root._reltime2(5)=.U; 
	if bigop(5)=1 then &root._reltime2(6)=&root(5); else if bigop(5)=0 then &root._reltime2(6)=.O; else if bigop(5)=. then &root._reltime2(6)=.U; 
	if bigop(6)=1 then &root._reltime2(7)=&root(6); else if bigop(6)=0 then &root._reltime2(7)=.O; else if bigop(6)=. then &root._reltime2(7)=.U; 
	if bigop(7)=1 then &root._reltime2(8)=&root(7); else if bigop(7)=0 then &root._reltime2(8)=.O; else if bigop(7)=. then &root._reltime2(8)=.U; 
	&root._reltime2(9)=.B;
	&root._reltime2(10)=.B; 
	&root._reltime2(11)=.B; 
	&root._reltime2(12)=.B;
	&root._reltime2(13)=.B; end; 
else if biground=14 then do; 
	if bigop(1)=1 then &root._reltime2(1)=&root(1); else if bigop(1)=0 then &root._reltime2(1)=.O; else if bigop(1)=. then &root._reltime2(1)=.U;  
	if bigop(2)=1 then &root._reltime2(2)=&root(2); else if bigop(2)=0 then &root._reltime2(2)=.O; else if bigop(2)=. then &root._reltime2(2)=.U; 
	if bigop(3)=1 then &root._reltime2(3)=&root(3); else if bigop(3)=0 then &root._reltime2(3)=.O; else if bigop(3)=. then &root._reltime2(3)=.U; 
	if bigop(4)=1 then &root._reltime2(4)=&root(4); else if bigop(4)=0 then &root._reltime2(4)=.O; else if bigop(4)=. then &root._reltime2(4)=.U; 
	if bigop(5)=1 then &root._reltime2(5)=&root(5); else if bigop(5)=0 then &root._reltime2(5)=.O; else if bigop(5)=. then &root._reltime2(5)=.U; 
	if bigop(6)=1 then &root._reltime2(6)=&root(6); else if bigop(6)=0 then &root._reltime2(6)=.O; else if bigop(6)=. then &root._reltime2(6)=.U; 
	if bigop(7)=1 then &root._reltime2(7)=&root(7); else if bigop(7)=0 then &root._reltime2(7)=.O; else if bigop(7)=. then &root._reltime2(7)=.U; 
	&root._reltime2(8)=.B; 
	&root._reltime2(9)=.B; 
	&root._reltime2(10)=.B; 
	&root._reltime2(11)=.B; 
	&root._reltime2(12)=.B;
	&root._reltime2(13)=.B; end; 
%mend; 
%reltime(opidtest,f2opidn f4opidn f6opidn f8opidn f10opidn f12opidn f14opidn);
/*proc print data=rel3(obs=10); 
var biground bigopid bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14 
f2opidn f4opidn f6opidn f8opidn f10opidn f12opidn f14opidn 
opidtest12b opidtest10b opidtest8b opidtest6b opidtest4b opidtest2b opidtest0 opidtest2a opidtest4a opidtest6a opidtest8a opidtest10a opidtest12a;
run;
proc print data=rel3(obs=10); 
var biground bigopid f2opidn f4opidn f6opidn f8opidn f10opidn f12opidn f14opidn 
opidtest12b_ opidtest10b_ opidtest8b_ opidtest6b_ opidtest4b_ opidtest2b_ opidtest0_ 
opidtest2a_ opidtest4a_ opidtest6a_ opidtest8a_ opidtest10a_ opidtest12a_;
run;*/



/******TARGET - AGE AT MARRIAGE******/

format tbday mmddyys10.;
agemar=round(((bigday-tbday)/365.25)/**12*/); 
label agemar="AGE AT TIME OF FIRST MARRIAGE/COHABITATION (Target)"; 
/*proc print data=rel3(obs=25); 
var agemar bigday tbday; 
run;*/ 



/********************************************************

     TRANSLATING RAW ITEMS ONTO RELATIVE TIME SCALE

*********************************************************/
/***********************************************************************
   Observer report of TARGET's HOSTILITY toward PARTNER 
************************************************************************/
%reltime(OThsP,FT245HSZ	FT445HSZ FT645HSZ FT845HSZ HT045HSZ HT205HSZ HT405HSZ);/* HT705HSZ); /**OThsP=root**/
Label OThsP0="Observer - Target HOSTILITY --> Partner (Time zero)"; /**0=time point**/
Label OThsP2A="Observer - Target HOSTILITY --> Partner (2 rounds after)"; /**2A=time point 2 years after**/
Label OThsP4A="Observer - Target HOSTILITY --> Partner (4 rounds after)"; 
Label OThsP6A="Observer - Target HOSTILITY --> Partner (6 rounds after)"; 
Label OThsP8A="Observer - Target HOSTILITY --> Partner (8 rounds after)";
Label OThsP10A="Observer - Target HOSTILITY --> Partner (10 rounds after)";

%reltime(OTacP,FT245ACZ FT445ACZ FT645ACZ FT845ACZ HT045ACZ HT205ACZ HT405ACZ); /* HT705ACZ);*/ 
Label OTacP0="Observer - Target ANGRY COERCION --> Partner (Time zero)"; 
Label OTacP2A="Observer - Target ANGRY COERCION --> Partner (2 rounds after)"; 
Label OTacP4A="Observer - Target ANGRY COERCION --> Partner (4 rounds after)"; 
Label OTacP6A="Observer - Target ANGRY COERCION --> Partner (6 rounds after)"; 
Label OTacP8A="Observer - Target ANGRY COERCION --> Partner (8 rounds after)"; 
Label OTacP10A="Observer - Target ANGRY COERCION --> Partner (10 rounds after)"; 

%reltime(OTehP,FT245EHZ FT445EHZ FT645EHZ FT845EHZ HT045EHZ HT205EHZ HT405EHZ); /* HT705EHZ);*/
Label OTehP0="Observer - Target ESCALATE HOSTILE --> Partner (Time zero)"; 
Label OTehP2A="Observer - Target ESCALATE HOSTILE --> Partner (2 rounds after)"; 
Label OTehP4A="Observer - Target ESCALATE HOSTILE --> Partner (4 rounds after)"; 
Label OTehP6A="Observer - Target ESCALATE HOSTILE --> Partner (6 rounds after)"; 
Label OTehP8A="Observer - Target ESCALATE HOSTILE --> Partner (8 rounds after)"; 
Label OTehP10A="Observer - Target ESCALATE HOSTILE --> Partner (10 rounds after)"; 

%reltime(OTrhP,FT245RHZ FT445RHZ FT645RHZ FT845RHZ HT045RHZ HT205RHZ HT405RHZ); /* HT705RHZ); */
Label OTrhP0="Observer - Target RECIPROCATE HOSTILE --> Partner (Time zero)"; 
Label OTrhP2A="Observer - Target RECIPROCATE HOSTILE --> Partner (2 rounds after)"; 
Label OTrhP4A="Observer - Target RECIPROCATE HOSTILE --> Partner (4 rounds after)"; 
Label OTrhP6A="Observer - Target RECIPROCATE HOSTILE --> Partner (6 rounds after)"; 
Label OTrhP8A="Observer - Target RECIPROCATE HOSTILE --> Partner (8 rounds after)"; 
Label OTrhP10A="Observer - Target RECIPROCATE HOSTILE --> Partner (10 rounds after)"; 

%reltime(OTanP,FT245ANZ FT445ANZ FT645ANZ FT845ANZ HT045ANZ HT205ANZ HT405ANZ); /* HT705ANZ); */
Label OTanP0="Observer - Target ANTISOCIAL --> Partner (Time zero)"; 
Label OTanP2A="Observer - Target ANTISOCIAL --> Partner (2 rounds after)"; 
Label OTanP4A="Observer - Target ANTISOCIAL --> Partner (4 rounds after)"; 
Label OTanP6A="Observer - Target ANTISOCIAL --> Partner (6 rounds after)"; 
Label OTanP8A="Observer - Target ANTISOCIAL --> Partner (8 rounds after)"; 
Label OTanP10A="Observer - Target ANTISOCIAL --> Partner (10 rounds after)"; 
/*proc print data=rel3(obs=20); 
var biground bigtype bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14 bigop17 FT245HSZ FT445HSZ FT645HSZ FT845HSZ HT045HSZ HT205HSZ HT405HSZ HT705HSZ
OThsP14B OThsP12B OThsP10B  OThsP8B  OThsP6B  OThsP4B  OThsP2B  OThsP0  OThsP2A  OThsP4A  OThsP6A  OThsP8A  OThsP10A  OThsP12A  OThsP14A
OThsP14B_ OThsP12B_ OThsP10B_ OThsP8B_ OThsP6B_ OThsP4B_ OThsP2B_ OThsP0_ OThsP2A_ OThsP4A_ OThsP6A_ OThsP8A_ OThsP10A_ OThsP12A_ OThsP14A_;
run;
proc means; 
var OThsP2B OThsP0  OThsP2A  OThsP4A  OThsP6A OThsP8A OThsP10A OThsP12A OThsP14A; 
run;   */

/************************************************************************
   Observer report of TARGET's SUPPORT toward PARTNER 
************************************************************************/
%reltime(OTwmP,FT245WMZ FT445WMZ FT645WMZ FT845WMZ HT045WMZ HT205WMZ HT405WMZ); /* HT705WMZ);*/
Label OTwmP0="Observer - Target WARMTH/SUPPORT --> Partner (Time zero)"; 
Label OTwmP2A="Observer - Target WARMTH/SUPPORT --> Partner (2 rounds after)"; 
Label OTwmP4A="Observer - Target WARMTH/SUPPORT --> Partner (4 rounds after)"; 
Label OTwmP6A="Observer - Target WARMTH/SUPPORT --> Partner (6 rounds after)"; 
Label OTwmP8A="Observer - Target WARMTH/SUPPORT --> Partner (8 rounds after)"; 
Label OTwmP10A="Observer - Target WARMTH/SUPPORT --> Partner (10 rounds after)"; 

%reltime(OTewP,FT245EWZ FT445EWZ FT645EWZ FT845EWZ HT045EWZ HT205EWZ HT405EWZ); /* HT705EWZ); */
Label OTewP0="Observer - Target ESCALATE WARMTH --> Partner (Time zero)"; 
Label OTewP2A="Observer - Target ESCALATE WARMTH --> Partner (2 rounds after)"; 
Label OTewP4A="Observer - Target ESCALATE WARMTH --> Partner (4 rounds after)"; 
Label OTewP6A="Observer - Target ESCALATE WARMTH --> Partner (6 rounds after)";
Label OTewP8A="Observer - Target ESCALATE WARMTH --> Partner (8 rounds after)";
Label OTewP10A="Observer - Target ESCALATE WARMTH --> Partner (10 rounds after)";

%reltime(OTrwP,FT245RWZ FT445RWZ FT645RWZ FT845RWZ HT045RWZ HT205RWZ HT405RWZ); /* HT705RWZ); */
Label OTrwP0="Observer - Target RECIPROCATE WARMTH --> Partner (Time zero)"; 
Label OTrwP2A="Observer - Target RECIPROCATE WARMTH --> Partner (2 rounds after)"; 
Label OTrwP4A="Observer - Target RECIPROCATE WARMTH --> Partner (4 rounds after)"; 
Label OTrwP6A="Observer - Target RECIPROCATE WARMTH --> Partner (6 rounds after)";
Label OTrwP8A="Observer - Target RECIPROCATE WARMTH --> Partner (8 rounds after)";
Label OTrwP10A="Observer - Target RECIPROCATE WARMTH --> Partner (10 rounds after)";

%reltime(OTarP,FT245ARZ FT445ARZ FT645ARZ FT845ARZ HT045ARZ HT205ARZ HT405ARZ); /* HT705ARZ); */
Label OTarP0="Observer - Target ASSERTIVENESS --> Partner (Time zero)"; 
Label OTarP2A="Observer - Target ASSERTIVENESS --> Partner (2 rounds after)"; 
Label OTarP4A="Observer - Target ASSERTIVENESS --> Partner (4 rounds after)"; 
Label OTarP6A="Observer - Target ASSERTIVENESS --> Partner (6 rounds after)";
Label OTarP8A="Observer - Target ASSERTIVENESS --> Partner (8 rounds after)";
Label OTarP10A="Observer - Target ASSERTIVENESS --> Partner (10 rounds after)";

%reltime(OTlrP,FT245LRZ FT445LRZ FT645LRZ FT845LRZ HT045LRZ HT205LRZ HT405LRZ); /* HT705LRZ); */
Label OTlrP0="Observer - Target LISTENER RESPONSIVENESS --> Partner (Time zero)"; 
Label OTlrP2A="Observer - Target LISTENER RESPONSIVENESS --> Partner (2 rounds after)"; 
Label OTlrP4A="Observer - Target LISTENER RESPONSIVENESS --> Partner (4 rounds after)"; 
Label OTlrP6A="Observer - Target LISTENER RESPONSIVENESS --> Partner (6 rounds after)";
Label OTlrP8A="Observer - Target LISTENER RESPONSIVENESS --> Partner (8 rounds after)";
Label OTlrP10A="Observer - Target LISTENER RESPONSIVENESS --> Partner (10 rounds after)";

%reltime(OTcoP,FT245COZ FT445COZ FT645COZ FT845COZ HT045COZ HT205COZ HT405COZ); /* HT705COZ); */
Label OTcoP0="Observer - Target COMMUNICATION --> Partner (Time zero)"; 
Label OTcoP2A="Observer - Target COMMUNICATION --> Partner (2 rounds after)"; 
Label OTcoP4A="Observer - Target COMMUNICATION --> Partner (4 rounds after)"; 
Label OTcoP6A="Observer - Target COMMUNICATION --> Partner (6 rounds after)";
Label OTcoP8A="Observer - Target COMMUNICATION --> Partner (8 rounds after)";
Label OTcoP10A="Observer - Target COMMUNICATION --> Partner (10 rounds after)";

%reltime(OTprP,FT245PRZ FT445PRZ FT645PRZ FT845PRZ HT045PRZ HT205PRZ HT405PRZ); /* HT705PRZ); */
Label OTprP0="Observer - Target PROSOCIAL --> Partner (Time zero)"; 
Label OTprP2A="Observer - Target PROSOCIAL --> Partner (2 rounds after)"; 
Label OTprP4A="Observer - Target PROSOCIAL --> Partner (4 rounds after)"; 
Label OTprP6A="Observer - Target PROSOCIAL --> Partner (6 rounds after)";
Label OTprP8A="Observer - Target PROSOCIAL --> Partner (8 rounds after)";
Label OTprP10A="Observer - Target PROSOCIAL --> Partner (10 rounds after)";

%reltime(OTrqP,FT245RQZ	FT445RQZ FT645RQZ FT845RQZ HT045RQZ	HX205RQZ HX405RQZ); /* HX205RQZ);  /* Adding RELATIONSHIP QUALITY (item)*/ 
Label OTrqP0="Observer - Target & PARTNER REL QUALITY (Time zero)"; 
Label OTrqP2A="Observer - Target & PARTNER REL QUALITY (2 rounds after)"; 
Label OTrqP4A="Observer - Target & PARTNER REL QUALITY (4 rounds after)"; 
Label OTrqP6A="Observer - Target & PARTNER REL QUALITY (6 rounds after)";
Label OTrqP8A="Observer - Target & PARTNER REL QUALITY (8 rounds after)";
Label OTrqP10A="Observer - Target & PARTNER REL QUALITY (10 rounds after)";



/**********************************************************************
    Observer Report of PARTNER's HOSTILITY toward TARGET 
************************************************************************/
%reltime (OPhsT, FZ245HST FZ445HST FZ645HST	FZ845HST HZ045HST HZ205HST HZ405HST); /* HZ705HST);	/*Obs: Partner hostility (item) --> Target */
Label OPhsT0="Observer - Partner HOSTILITY --> Target (Time zero)"; 
Label OPhsT2A="Observer - Partner HOSTILITY --> Target (2 rounds after)"; 
Label OPhsT4A="Observer - Partner HOSTILITY --> Target (4 rounds after)"; 
Label OPhsT6A="Observer - Partner HOSTILITY --> Target (6 rounds after)"; 
Label OPhsT8A="Observer - Partner HOSTILITY --> Target (8 rounds after)"; 
Label OPhsT10A="Observer - Partner HOSTILITY --> Target (10 rounds after)"; 

%reltime (OPacT, FZ245ACT FZ445ACT FZ645ACT	FZ845ACT HZ045ACT HZ205ACT HZ405ACT); /* HZ705ACT);	/*Obs: Partner angry coercion (item) --> Target */
Label OPacT0="Observer - Partner ANGRY COERCION --> Target (Time zero)"; 
Label OPacT2A="Observer - Partner ANGRY COERCION --> Target (2 rounds after)"; 
Label OPacT4A="Observer - Partner ANGRY COERCION --> Target (4 rounds after)"; 
Label OPacT6A="Observer - Partner ANGRY COERCION --> Target (6 rounds after)"; 
Label OPacT8A="Observer - Partner ANGRY COERCION --> Target (8 rounds after)"; 
Label OPacT10A="Observer - Partner ANGRY COERCION --> Target (10 rounds after)"; 

%reltime (OPehT, FZ245EHT FZ445EHT FZ645EHT	FZ845EHT HZ045EHT HZ205EHT HZ405EHT); /* HZ705EHT);	/*Obs: Partner escalate hostile (item) --> Target */
Label OPehT0="Observer - Partner ESCALATE HOSTILE --> Target (Time zero)"; 
Label OPehT2A="Observer - Partner ESCALATE HOSTILE --> Target (2 rounds after)"; 
Label OPehT4A="Observer - Partner ESCALATE HOSTILE --> Target (4 rounds after)"; 
Label OPehT6A="Observer - Partner ESCALATE HOSTILE --> Target (6 rounds after)"; 
Label OPehT8A="Observer - Partner ESCALATE HOSTILE --> Target (8 rounds after)"; 
Label OPehT10A="Observer - Partner ESCALATE HOSTILE --> Target (10 rounds after)"; 

%reltime (OPrhT, FZ245RHT FZ445RHT FZ645RHT	FZ845RHT HZ045RHT HZ205RHT HZ405RHT); /* HZ705RHT);	/*Obs: Partner reciprocate hostile (item) --> Target */
Label OPrhT0="Observer - Partner RECIPROCATE HOSTILE --> Target (Time zero)"; 
Label OPrhT2A="Observer - Partner RECIPROCATE HOSTILE --> Target (2 rounds after)"; 
Label OPrhT4A="Observer - Partner RECIPROCATE HOSTILE --> Target (4 rounds after)"; 
Label OPrhT6A="Observer - Partner RECIPROCATE HOSTILE --> Target (6 rounds after)"; 
Label OPrhT8A="Observer - Partner RECIPROCATE HOSTILE --> Target (8 rounds after)"; 
Label OPrhT10A="Observer - Partner RECIPROCATE HOSTILE --> Target (10 rounds after)"; 

%reltime (OPanT, FZ245ANT FZ445ANT FZ645ANT	FZ845ANT HZ045ANT HZ205ANT HZ405ANT); /* HZ705ANT);	/*Obs: Partner antisocial (item) --> Target */
Label OPanT0="Observer - Partner ANTISOCIAL --> Target (Time zero)"; 
Label OPanT2A="Observer - Partner ANTISOCIAL --> Target (2 rounds after)"; 
Label OPanT4A="Observer - Partner ANTISOCIAL --> Target (4 rounds after)"; 
Label OPanT6A="Observer - Partner ANTISOCIAL --> Target (6 rounds after)"; 
Label OPanT8A="Observer - Partner ANTISOCIAL --> Target (8 rounds after)"; 
Label OPanT10A="Observer - Partner ANTISOCIAL --> Target (10 rounds after)"; 


/********************************************************************
    Observer Report of PARTNER's SUPPORT toward TARGET 
*********************************************************************/		
%reltime (OPwmT, FZ245WMT FZ445WMT FZ645WMT	FZ845WMT HZ045WMT HZ205WMT HZ405WMT); /* HZ705WMT);	/*Obs: Partner warmth/support (item) --> Target */
Label OPwmT0="Observer - Partner WARMTH/SUPPORT --> Target (Time zero)"; 
Label OPwmT2A="Observer - Partner WARMTH/SUPPORT --> Target (2 rounds after)"; 
Label OPwmT4A="Observer - Partner WARMTH/SUPPORT --> Target (4 rounds after)"; 
Label OPwmT6A="Observer - Partner WARMTH/SUPPORT --> Target (6 rounds after)";
Label OPwmT8A="Observer - Partner WARMTH/SUPPORT --> Target (8 rounds after)";
Label OPwmT10A="Observer - Partner WARMTH/SUPPORT --> Target (10 rounds after)";

%reltime (OPewT, FZ245EWT FZ445EWT FZ645EWT	FZ845EWT HZ045EWT HZ205EWT HZ405EWT); /* HZ705EWT); 	/*Obs: Partner escalate warmth (item) --> Target */
Label OPewT0="Observer - Partner ESCALATE WARMTH --> Target (Time zero)"; 
Label OPewT2A="Observer - Partner ESCALATE WARMTH --> Target (2 rounds after)"; 
Label OPewT4A="Observer - Partner ESCALATE WARMTH --> Target (4 rounds after)"; 
Label OPewT6A="Observer - Partner ESCALATE WARMTH --> Target (6 rounds after)";
Label OPewT8A="Observer - Partner ESCALATE WARMTH --> Target (8 rounds after)";
Label OPewT10A="Observer - Partner ESCALATE WARMTH --> Target (10 rounds after)";

%reltime (OPrwT, FZ245RWT FZ445RWT FZ645RWT	FZ845RWT HZ045RWT HZ205RWT HZ405RWT); /* HZ705RWT);	/*Obs: Partner reciprocate warmth (item) --> Target */
Label OPrwT0="Observer - Partner RECIPROCATE WARMTH --> Target (Time zero)"; 
Label OPrwT2A="Observer - Partner RECIPROCATE WARMTH --> Target (2 rounds after)"; 
Label OPrwT4A="Observer - Partner RECIPROCATE WARMTH --> Target (4 rounds after)"; 
Label OPrwT6A="Observer - Partner RECIPROCATE WARMTH --> Target (6 rounds after)";
Label OPrwT8A="Observer - Partner RECIPROCATE WARMTH --> Target (8 rounds after)";
Label OPrwT10A="Observer - Partner RECIPROCATE WARMTH --> Target (10 rounds after)";

%reltime (OParT, FZ245ART FZ445ART FZ645ART	FZ845ART HZ045ART HZ205ART HZ405ART); /* HZ705ART); 	/*Obs: Partner assertiveness (item) --> Target */
Label OParT0="Observer - Partner ASSERTIVENESS --> Target (Time zero)"; 
Label OParT2A="Observer - Partner ASSERTIVENESS --> Target (2 rounds after)"; 
Label OParT4A="Observer - Partner ASSERTIVENESS --> Target (4 rounds after)"; 
Label OParT6A="Observer - Partner ASSERTIVENESS --> Target (6 rounds after)";
Label OParT8A="Observer - Partner ASSERTIVENESS --> Target (8 rounds after)";
Label OParT10A="Observer - Partner ASSERTIVENESS --> Target (10 rounds after)";

%reltime (OPlrT, FZ245LRT FZ445LRT FZ645LRT	FZ845LRT HZ045LRT HZ205LRT HZ405LRT); /* HZ705LRT); 	/*Obs: Partner listener responsiveness (item) --> Target */
Label OPlrT0="Observer - Partner LISTENER RESPONSIVENESS --> Target (Time zero)"; 
Label OPlrT2A="Observer - Partner LISTENER RESPONSIVENESS --> Target (2 rounds after)"; 
Label OPlrT4A="Observer - Partner LISTENER RESPONSIVENESS --> Target (4 rounds after)"; 
Label OPlrT6A="Observer - Partner LISTENER RESPONSIVENESS --> Target (6 rounds after)";
Label OPlrT8A="Observer - Partner LISTENER RESPONSIVENESS --> Target (8 rounds after)";
Label OPlrT10A="Observer - Partner LISTENER RESPONSIVENESS --> Target (10 rounds after)";

%reltime (OPcoT, FZ245COT FZ445COT FZ645COT	FZ845COT HZ045COT HZ205COT HZ405COT); /* HZ705COT); 	/*Obs: Partner communication (item) --> Target */
Label OPcoT0="Observer - Partner COMMUNICATION --> Target (Time zero)"; 
Label OPcoT2A="Observer - Partner COMMUNICATION --> Target (2 rounds after)"; 
Label OPcoT4A="Observer - Partner COMMUNICATION --> Target (4 rounds after)"; 
Label OPcoT6A="Observer - Partner COMMUNICATION --> Target (6 rounds after)";
Label OPcoT8A="Observer - Partner COMMUNICATION --> Target (8 rounds after)";
Label OPcoT10A="Observer - Partner COMMUNICATION --> Target (10 rounds after)";

%reltime (OPprT, FZ245PRT FZ445PRT FZ645PRT	FZ845PRT HZ045PRT HZ205PRT HZ405PRT); /* HZ705PRT); 	/*Obs: Partner prosocial (item) --> Target */
Label OPprT0="Observer - Partner PROSOCIAL --> Target (Time zero)"; 
Label OPprT2A="Observer - Partner PROSOCIAL --> Target (2 rounds after)"; 
Label OPprT4A="Observer - Partner PROSOCIAL --> Target (4 rounds after)"; 
Label OPprT6A="Observer - Partner PROSOCIAL --> Target (6 rounds after)";
Label OPprT8A="Observer - Partner PROSOCIAL --> Target (8 rounds after)";
Label OPprT10A="Observer - Partner PROSOCIAL --> Target (10 rounds after)";

%reltime (OPrqT, FT245RQZ FT445RQZ FT645RQZ	FT845RQZ HT045RQZ HX205RQZ HX405RQZ); /* HZ705RQZ);	/*Obs: Partner & Target relationship quality (item)*/
Label OPrqT0="Observer - Partner & Target REL QUALITY (Time zero)"; 
Label OPrqT2A="Observer - Partner & Target REL QUALITY (2 rounds after)"; 
Label OPrqT4A="Observer - Partner & Target REL QUALITY (4 rounds after)"; 
Label OPrqT6A="Observer - Partner & Target REL QUALITY (6 rounds after)";
Label OPrqT8A="Observer - Partner & Target REL QUALITY (8 rounds after)";
Label OPrqT10A="Observer - Partner & Target REL QUALITY (10 rounds after)";
/*  proc freq; 
tables FZ245WMT FZ445WMT FZ645WMT FZ845WMT HZ045WMT HZ205WMT HZ405WMT;
run;
proc print data=rel3(obs=20); 
var biground bigtype bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 FZ245WMT FZ445WMT FZ645WMT FZ845WMT HZ045WMT HZ205WMT HZ405WMT
 OPwmT14B  OPwmT12B  OPwmT10B   OPwmT8B    OPwmT6B   OPwmT4B   OPwmT2B   OPwmT0   OPwmT2A   OPwmT4A   OPwmT6A   OPwmT8A   OPwmT10A  OPwmT12A  OPwmT14A
 OPwmT14B_ OPwmT12B_ OPwmT10B_  OPwmT8B_   OPwmT6B_  OPwmT4B_  OPwmT2B_  OPwmT0_  OPwmT2A_  OPwmT4A_  OPwmT6A_  OPwmT8A_  OPwmT10A_ OPwmT12A_ OPwmT14A_;
run;  
proc means; 
var OPwmT4B  OPwmT2B OPwmT0  OPwmT2A  OPwmT4A  OPwmT6A OPwmT8A OPwmT10A OPwmT12A OPwmT14A; 
run; */



/************************************************************************
    PARTNER report of TARGET's HOSTILITY toward PARTNER 
************************************************************************/
%reltime(PTanP,FZc241043 FZc441045 FZc641045 FZc841047 HZc041044 HZc241041 HZc441044); /* HZc731025); */
Label PTanP0="Partner - Target ANGRY at --> Partner (Time zero)"; 
Label PTanP2A="Partner - Target ANGRY at --> Partner (2 rounds after)"; 
Label PTanP4A="Partner - Target ANGRY at --> Partner (4 rounds after)"; 
Label PTanP6A="Partner - Target ANGRY at --> Partner (6 rounds after)"; 
Label PTanP8A="Partner - Target ANGRY at --> Partner (8 rounds after)"; 
Label PTanP10A="Partner - Target ANGRY at --> Partner (10 rounds after)"; 

%reltime(PTcrP,FZc241047 FZc441049 FZc641049 FZc841051 HZc041048 HZc241045 HZc441048); /* HZc731029); 	*/
Label PTcrP0="Partner - Target CRITICIZE --> Partner (Time zero)"; 
Label PTcrP2A="Partner - Target CRITICIZE --> Partner (2 rounds after)"; 
Label PTcrP4A="Partner - Target CRITICIZE --> Partner (4 rounds after)"; 
Label PTcrP6A="Partner - Target CRITICIZE --> Partner (6 rounds after)"; 
Label PTcrP8A="Partner - Target CRITICIZE --> Partner (8 rounds after)"; 
Label PTcrP10A="Partner - Target CRITICIZE --> Partner (10 rounds after)"; 

%reltime(PTylP,FZc241048 FZc441050 FZc641050 FZc841052 HZc041049 HZc241046 HZc441049); /* HZc731030);*/
Label PTylP0="Partner - Target YELL at --> Partner (Time zero)"; 
Label PTylP2A="Partner - Target YELL at --> Partner (2 rounds after)"; 
Label PTylP4A="Partner - Target YELL at --> Partner (4 rounds after)"; 
Label PTylP6A="Partner - Target YELL at --> Partner (6 rounds after)"; 
Label PTylP8A="Partner - Target YELL at --> Partner (8 rounds after)"; 
Label PTylP10A="Partner - Target YELL at --> Partner (10 rounds after)"; 

%reltime(PThtP,FZc241056 FZc441058 FZc641058 FZc841060 HZc041057 HZc241054 HZc441057); /* HZc731038);*/
Label PThtP0="Partner - Target HIT --> Partner (Time zero)"; 
Label PThtP2A="Partner - Target HIT --> Partner (2 rounds after)"; 
Label PThtP4A="Partner - Target HIT --> Partner (4 rounds after)"; 
Label PThtP6A="Partner - Target HIT --> Partner (6 rounds after)"; 
Label PThtP8A="Partner - Target HIT --> Partner (8 rounds after)"; 
Label PThtP10A="Partner - Target HIT --> Partner (10 rounds after)"; 

%reltime(PTarP,FZc241058 FZc441060 FZc641060 FZc841062 HZc041059 HZc241056 HZc441059); /* HZc731040);*/
Label PTarP0="Partner - Target ARGUE --> Partner (Time zero)"; 
Label PTarP2A="Partner - Target ARGUE --> Partner (2 rounds after)"; 
Label PTarP4A="Partner - Target ARGUE --> Partner (4 rounds after)"; 
Label PTarP6A="Partner - Target ARGUE --> Partner (6 rounds after)"; 
Label PTarP8A="Partner - Target ARGUE --> Partner (8 rounds after)"; 
Label PTarP10A="Partner - Target ARGUE --> Partner (10 rounds after)"; 


/************************************************************************
    PARTNER report of TARGET's SUPPORT toward PARTNER 								
************************************************************************/
%reltime(PTopP,FZc241044 FZc441046 FZc641046 FZc841048 HZc041045 HZc241042 HZc441045);  /*Partner: Target --> ask partner's OPINION */ 
%reltime(PTlsP,FZc241045 FZc441047 FZc641047 FZc841049 HZc041046 HZc241043 HZc441046);  /*Partner: Target --> LISTEN carefully */
%reltime(PTcaP,FZc241046 FZc441048 FZc641048 FZc841050 HZc041047 HZc241044 HZc441047);  /*Partner: Target --> really CARES */
%reltime(PTafP,FZc241049 FZc441051 FZc641051 FZc841053 HZc041050 HZc241047 HZc441050);  /*Partner: Target --> act loving and AFFECTIONATE */
%reltime(PTapP,FZc241052 FZc441054 FZc641054 FZc841056 HZc041053 HZc241050 HZc441053);  /*Partner: Target --> APPRECIATES */
%reltime(PThpP,FZc241053 FZc441055 FZc641055 FZc841057 HZc041054 HZc241051 HZc441054);  /*Partner: Target --> HELP partner */
%reltime(PTspP,FZc241059 FZc441061 FZc641061 FZc841063 HZc041060 HZc241057 HZc441060);  /*Partner: Target --> SUPPORTIVE and understanding */
%reltime(PTlvP,FZc241064 FZc441066 FZc641066 FZc841068 HZc041065 HZc241062 HZc441065);  /*Partner: Target --> LOVES partner */



/******************************************************************************
    PARTNER report of PARTNER's HOSTILITY toward TARGET 
*******************************************************************************/
%reltime(PPanT, FZc241065 FZc441075 FZc641075 FZc841114 HZc041111 HZc241092 HZc441095);		/*Partner: Partner --> angry at target */ 
Label PPanT0="Partner - Partner ANGRY at --> Target (Time zero)"; 
Label PPanT2A="Partner - Partner ANGRY at --> Target (2 rounds after)"; 
Label PPanT4A="Partner - Partner ANGRY at --> Target (4 rounds after)"; 
Label PPanT6A="Partner - Partner ANGRY at --> Target (6 rounds after)"; 
Label PPanT8A="Partner - Partner ANGRY at --> Target (8 rounds after)"; 

%reltime (PPcrT, FZc241067 FZc441077 FZc641077 FZc841118 HZc041115 HZc241096 HZc441099);	/*Partner: Partner --> criticizes target */
Label PPcrT0="Partner - Partner CRITICIZE --> Target (Time zero)"; 
Label PPcrT2A="Partner - Partner CRITICIZE --> Target (2 rounds after)"; 
Label PPcrT4A="Partner - Partner CRITICIZE --> Target (4 rounds after)"; 
Label PPcrT6A="Partner - Partner CRITICIZE --> Target (6 rounds after)"; 
Label PPcrT8A="Partner - Partner CRITICIZE --> Target (8 rounds after)"; 

%reltime (PPylT, FZc241068 FZc441078 FZc641078 FZc841119 HZc041116 HZc241097 HZc441100);	/*Partner: Partner --> yells at target */
Label PPylT0="Partner - Partner YELL at --> Target (Time zero)"; 
Label PPylT2A="Partner - Partner YELL at --> Target (2 rounds after)"; 
Label PPylT4A="Partner - Partner YELL at --> Target (4 rounds after)"; 
Label PPylT6A="Partner - Partner YELL at --> Target (6 rounds after)"; 
Label PPylT8A="Partner - Partner YELL at --> Target (8 rounds after)"; 

%reltime (PPhtT, FZc241073 FZc441083 FZc641083 FZc841127 HZc041124 HZc241105 HZc441108);	/*Partner: Partner --> hits target */
Label PPhtT0="Partner - Partner HIT --> Target (Time zero)"; 
Label PPhtT2A="Partner - Partner HIT --> Target (2 rounds after)"; 
Label PPhtT4A="Partner - Partner HIT --> Target (4 rounds after)"; 
Label PPhtT6A="Partner - Partner HIT --> Target (6 rounds after)"; 
Label PPhtT8A="Partner - Partner HIT --> Target (8 rounds after)"; 

%reltime (PParT, FZc241072 FZc441082 FZc641082 FZc841129 HZc041126 HZc241107 HZc441110); 	/*Partner: Partner --> argues with target */
Label PParT0="Partner - Partner ARGUE --> Target (Time zero)"; 
Label PParT2A="Partner - Partner ARGUE --> Target (2 rounds after)"; 
Label PParT4A="Partner - Partner ARGUE --> Target (4 rounds after)"; 
Label PParT6A="Partner - Partner ARGUE --> Target (6 rounds after)"; 
Label PParT8A="Partner - Partner ARGUE --> Target (8 rounds after)"; 


/************************************************************************
    PARTNER REPORT OF PARTNER'S SUPPORT TOWARD TARGET 
************************************************************************/
%reltime(PPcaT, FZc241066 FZc441076 FZc641076 FZc841117 HZc041114 HZc241095 HZc441098);		/*Partner: Partner --> cares about target */ 
%reltime(PPafT, FZc241069 FZc441079 FZc641079 FZc841120 HZc041117 HZc241098 HZc441101);		/*Partner: Partner --> affectionate toward target */ 
%reltime(PPapT, FZc241070 FZc441080 FZc641080 FZc841123 HZc041120 HZc241101 HZc441104);		/*Partner: Partner --> appreciates target */ 
%reltime(PPhpT, FZc241071 FZc441081 FZc641081 FZc841124 HZc041121 HZc241102 HZc441105);		/*Partner: Partner --> help target */ 


/************************************************************************
    TARGET report of TARGET'S HOSTILITY toward PARTNER 
************************************************************************/
%reltime(TTanP, FTc241065 FTc441075 FTc641075 FTc841114 HTc041111 HTc241092 HTc441095);		/*Target: Target --> how often get angry at partner */
Label TTanP0="Target - Target ANGRY at --> Partner (Time zero)"; 
Label TTanP2A="Target - Target ANGRY at --> Partner (2 rounds after)"; 
Label TTanP4A="Target - Target ANGRY at --> Partner (4 rounds after)"; 
Label TTanP6A="Target - Target ANGRY at --> Partner (6 rounds after)"; 
Label TTanP8A="Target - Target ANGRY at --> Partner (8 rounds after)"; 

%reltime(TTcrP, FTc241067 FTc441077 FTc641077 FTc841118 HTc041115 HTc241096 HTc441099);		/*Target: Target --> how often criticize partner */
Label TTcrP0="Target - Target CRITICIZE --> Partner (Time zero)"; 
Label TTcrP2A="Target - Target CRITICIZE --> Partner (2 rounds after)"; 
Label TTcrP4A="Target - Target CRITICIZE --> Partner (4 rounds after)"; 
Label TTcrP6A="Target - Target CRITICIZE --> Partner (6 rounds after)"; 
Label TTcrP8A="Target - Target CRITICIZE --> Partner (8 rounds after)"; 

%reltime(TTylP, FTc241068 FTc441078 FTc641078 FTc841119 HTc041116 HTc241097 HTc441100);		/*Target: Target --> yell at partner */
Label TTylP0="Target - Target YELL at --> Partner (Time zero)"; 
Label TTylP2A="Target - Target YELL at --> Partner (2 rounds after)"; 
Label TTylP4A="Target - Target YELL at --> Partner (4 rounds after)"; 
Label TTylP6A="Target - Target YELL at --> Partner (6 rounds after)"; 
Label TTylP8A="Target - Target YELL at --> Partner (8 rounds after)"; 

%reltime(TThtP, FTc241073 FTc441083 FTc641083 FTc841127 HTc041124 HTc241105 HTc441108);		/*Target: Target --> hit partner */
Label TThtP0="Target - Target HIT --> Partner (Time zero)"; 
Label TThtP2A="Target - Target HIT --> Partner (2 rounds after)"; 
Label TThtP4A="Target - Target HIT --> Partner (4 rounds after)"; 
Label TThtP6A="Target - Target HIT --> Partner (6 rounds after)"; 
Label TThtP8A="Target - Target HIT --> Partner (8 rounds after)"; 

%reltime(TTarP, FTc241072 FTc441082 FTc641082 FTc841129 HTc041126 HTc241107 HTc441110);		/*Target: Target --> argue with partner */
Label TTarP0="Target - Target ARGUE --> Partner (Time zero)"; 
Label TTarP2A="Target - Target ARGUE --> Partner (2 rounds after)"; 
Label TTarP4A="Target - Target ARGUE --> Partner (4 rounds after)"; 
Label TTarP6A="Target - Target ARGUE --> Partner (6 rounds after)"; 
Label TTarP8A="Target - Target ARGUE --> Partner (8 rounds after)"; 

/**(below:) bigop(yr)=whether is same op or not; without underscore=mar/coh; with underscore=mar/coh & dating**/

/* proc print data=REL3 (obs=20); 
 var biground bigtype bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 BIGOP14   
  FTc241068 FTc441078 FTc641078 FTc841119 HTc041116 HTc241097 HTc441100
  TTylP12B  TTylP10B   TTylP8B    TTylP6B   TTylP4B   TTylP2B   TTylP0   TTylP2A   TTylP4A   TTylP6A   TTylP8A   TTylP10A  TTylP12A 
  TTylP12B_ TTylP10B_  TTylP8B_   TTylP6B_  TTylP4B_  TTylP2B_  TTylP0_  TTylP2A_  TTylP4A_  TTylP6A_  TTylP8A_  TTylP10A_ TTylP12A_ ;
run;  
proc means; 
var TTylP2b TTylP0  TTylP2A  TTylP4A  TTylP6A TTylP8A TTylP10A TTylP12a; 
run;  /


/************************************************************************
    TARGET report of TARGET's SUPPORT toward PARTNER 
************************************************************************/
%reltime(TTcaP, FTc241066 FTc441076 FTc641076 FTc841117 HTc041114 HTc241095 HTc441098);		/*Target: Target --> cares about partner */ 
%reltime(TTafP, FTc241069 FTc441079 FTc641079 FTc841120 HTc041117 HTc241098 HTc441101);		/*Target: Target --> affectionate toward partner */ 
%reltime(TTapP, FTc241070 FTc441080 FTc641080 FTc841123 HTc041120 HTc241101 HTc441104);		/*Target: Target --> appreciates partner */ 
%reltime(TThpP, FTc241071 FTc441081 FTc641081 FTc841124 HTc041121 HTc241102 HTc441105);		/*Target: Target --> help partner */



/************************************************************************
    TARGET report of PARTNER's HOSTILITY toward TARGET 
************************************************************************/
%reltime(TPanT, FTc241043 FTc441045 FTc641045 FTc841047 HTc041044 HTc241041 HTc441044);		/*Target: Partner --> angry at target */
Label TPanT0="Target - Partner ANGRY at --> Target (Time zero)"; 
Label TPanT2A="Target - Partner ANGRY at --> Target (2 rounds after)"; 
Label TPanT4A="Target - Partner ANGRY at --> Target (4 rounds after)"; 
Label TPanT6A="Target - Partner ANGRY at --> Target (6 rounds after)"; 
Label TPanT8A="Target - Partner ANGRY at --> Target (8 rounds after)"; 

%reltime(TPcrT, FTc241047 FTc441049 FTc641049 FTc841051 HTc041048 HTc241045 HTc441048);		/*Target: Partner --> criticize target */
Label TPcrT0="Target - Partner CRITICIZE --> Target (Time zero)"; 
Label TPcrT2A="Target - Partner CRITICIZE --> Target (2 rounds after)"; 
Label TPcrT4A="Target - Partner CRITICIZE --> Target (4 rounds after)"; 
Label TPcrT6A="Target - Partner CRITICIZE --> Target (6 rounds after)"; 
Label TPcrT8A="Target - Partner CRITICIZE --> Target (8 rounds after)"; 

%reltime(TPylT, FTc241048 FTc441050 FTc641050 FTc841052 HTc041049 HTc241046 HTc441049);		/*Target: Partner --> yell at target */
Label TPylT0="Target - Partner YELL at --> Target (Time zero)"; 
Label TPylT2A="Target - Partner YELL at --> Target (2 rounds after)"; 
Label TPylT4A="Target - Partner YELL at --> Target (4 rounds after)"; 
Label TPylT6A="Target - Partner YELL at --> Target (6 rounds after)"; 
Label TPylT8A="Target - Partner YELL at --> Target (8 rounds after)"; 

%reltime(TPhtT, FTc241056 FTc441058 FTc641058 FTc841060 HTc041057 HTc241054 HTc441057);		/*Target: Partner --> hit target */
Label TPhtT0="Target - Partner HIT --> Target (Time zero)"; 
Label TPhtT2A="Target - Partner HIT --> Target (2 rounds after)"; 
Label TPhtT4A="Target - Partner HIT --> Target (4 rounds after)"; 
Label TPhtT6A="Target - Partner HIT --> Target (6 rounds after)"; 
Label TPhtT8A="Target - Partner HIT --> Target (8 rounds after)"; 

%reltime(TParT, FTc241058 FTc441060 FTc641060 FTc841062 HTc041059 HTc241056 HTc441059);		/*Target: Partner --> argue with target */
Label TParT0="Target - Partner ARGUE with --> Target (Time zero)"; 
Label TParT2A="Target - Partner ARGUE with --> Target (2 rounds after)"; 
Label TParT4A="Target - Partner ARGUE with --> Target (4 rounds after)"; 
Label TParT6A="Target - Partner ARGUE with --> Target (6 rounds after)"; 
Label TParT8A="Target - Partner ARGUE with --> Target (8 rounds after)"; 


/************************************************************************
    TARGET report of PARTNER's SUPPORT toward TARGET 
************************************************************************/
%reltime(TPopT, FTc241044 FTc441046 FTc641046 FTc841048 HTc041045 HTc241042 HTc441045);		/*Target: Partner --> asks target for opinion */ 
%reltime(TPlsT, FTc241045 FTc441047 FTc641047 FTc841049 HTc041046 HTc241043 HTc441046);		/*Target: Partner --> listen carefully to target's point of view */
%reltime(TPcaT, FTc241046 FTc441048 FTc641048 FTc841050 HTc041047 HTc241044 HTc441047);		/*Target: Partner --> cares about target */ 
%reltime(TPafT, FTc241049 FTc441051 FTc641051 FTc841053 HTc041050 HTc241047 HTc441050);		/*Target: Partner --> affectionate toward target */ 
%reltime(TPapT, FTc241052 FTc441054 FTc641054 FTc841056 HTc041053 HTc241050 HTc441053);		/*Target: Partner --> appreciates target */ 
%reltime(TPhpT, FTc241053 FTc441055 FTc641055 FTc841057 HTc041054 HTc241051 HTc441054);		/*Target: Partner --> help target */ 
%reltime(TPspT, FTc241059 FTc441061 FTc641061 FTc841063 HTc041060 HTc241057 HTc441060);		/*Target: Partner --> act supportive and understanding */ 
%reltime(TPlvT, FTc241064 FTc441066 FTc641066 FTc841068 HTc041065 HTc241062 HTc441065);		/*Target: Partner --> tell he/she loves target */ 

/************************************************************************
    TARGET report of TARGET's RELATIONSHIP INSTABILITY toward PARTNER 
************************************************************************/
%reltime(TTseP, FTc241033 FTc441035 FTc641035 FTc841034 HTc041030 HTc241027 HTc441030);		/*Target: Partner --> suggested idea of ending relationship */ 
%reltime(TTfrP, FTc241034 FTc441036 FTc641036 FTc841035 HTc041031 HTc241028 HTc441031);		/*Target: Partner --> discuss with close friend */
%reltime(TTtrP, FTc241035 FTc441037 FTc641037 FTc841036 HTc041032 HTc241029 HTc441032);		/*Target: Partner --> thought might be in trouble */ 
%reltime(TTotP, FTc241036 FTc441038 FTc641038 FTc841037 HTc041033 HTc241030 HTc441033);		/*Target: Partner --> talked to attorney/others */ 
%reltime(TTthP, FTc241037 FTc441039 FTc641039 FTc841038 HTc041034 HTc241031 HTc441034);		/*Target: Partner --> thought crossed mind */ 

/************************************************************************
    PARTNER report of PARTNER's RELATIONSHIP INSTABILITY toward TARGET 
************************************************************************/
%reltime(PPseT, FZc241033 FZc441035 FZc641035 FZc841034 HZc041030 HZc241027 HZc441030);		/*Partner: Target --> suggested idea of ending relationship */ 
%reltime(PPfrT, FZc241034 FZc441036 FZc641036 FZc841035 HZc041031 HZc241028 HZc441031);		/*Partner: Target --> discuss with close friend */
%reltime(PPtrT, FZc241035 FZc441037 FZc641037 FZc841036 HZc041032 HZc241029 HZc441032);		/*Partner: Target --> thought might be in trouble */ 
%reltime(PPotT, FZc241036 FZc441038 FZc641038 FZc841037 HZc041033 HZc241030 HZc441033);		/*Partner: Target --> talked to attorney/others */ 
%reltime(PPthT, FZc241037 FZc441039 FZc641039 FZc841038 HZc041034 HZc241031 HZc441034);		/*Partner: Target --> thought crossed mind */ 

run;



/**********************************************************
    		VIDEO DISCUSSION QUESTIONNAIRES
***********************************************************/
data rel4;
Set rel3;
array bigop(7) bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14; 

/***  The following arrays will RECODE 9 as MISSING For all variables in VIDEO DISCUSSION Q., all waves  ***/
Array vid95t(22) FT203054-FT203075;
Do aa=1 to 22;
  if vid95t(aa) > 6 then vid95t(aa) = . ;
end;
Drop aa;

Array vid95p(22) FZ203057-FZ203078;
Do bb=1 to 22;
  if vid95p(bb) > 6 then vid95p(bb) = . ;
end;
Drop bb;

Array vid97t(22) FX403051-FX403072; 
Do cc=1 to 22;
  if vid97t(cc) > 6 then vid97t(cc) = . ;
end;
Drop cc;

Array vid97p(22) FZ403054-FZ403075; 
Do dd=1 to 22;
  if vid97p(dd) > 6 then vid97p(dd) = . ;
end;
Drop dd;

Array vid99t(22) FT603051-FT603072;
Do ee=1 to 22;
  if vid99t(ee) > 6 then vid99t(ee) = . ;
end;
Drop ee;

Array vid99p(22)FZ603051-FZ603072;  
Do ff=1 to 22;
  if vid99p(ff) > 6 then vid99p(ff) = . ;
end;
Drop ff;

Array vid01t(22) FT801001-FT801022;  
Do gg=1 to 22;
  if vid01t(gg) > 6 then vid01t(gg) = . ;
end;
Drop gg;

Array vid01p(22) FZ801001-FZ801022;  
Do hh=1 to 22;
  if vid01p(hh) > 6 then vid01p(hh) = . ;
end;
Drop hh;

Array vid03t(22) HT002001-HT002022; 
Do ii=1 to 22;
  if vid03t(ii) > 6 then vid03t(ii) = . ;
end;
Drop ii;

Array vid03p(22) HZ002001-HZ002022; 
Do jj=1 to 22;
  if vid03p(jj) > 6 then vid03p(jj) = . ;
end;
Drop jj;

Array vid05t(22) HT202001-HT202022;  
Do kk=1 to 22;
  if vid05t(kk) > 6 then vid05t(kk) = . ;
end;
Drop kk;

Array vid05p(22) HZ202001-HZ202022; 
Do ll=1 to 22;
  if vid05p(ll) > 6 then vid05p(ll) = . ;
end;
Drop ll;

Array vid07t(22) HT402001-HT402022;  
Do mm=1 to 22;
  if vid07t(mm) > 6 then vid07t(mm) = . ;
end;
Drop mm;

Array vid07p(22) HZ402001-HZ402022; 
Do nn=1 to 22;
  if vid07p(nn) > 6 then vid07p(nn) = . ;
end;
Drop nn;



/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE (Time-shifted, raw items "root"): 
		1. Target report of PARTNER's HOSTILITY toward target during obs. task.
**************************************************************************************/
%reltime(TPcrTX, FT203054 FX403051 FT603051 FT801001 HT002001 HT202001 HT402001); 
%reltime(TPanTX, FT203057 FX403054 FT603054 FT801004 HT002004 HT202004 HT402004);
%reltime(TParTX, FT203059 FX403056 FT603056 FT801006 HT002006 HT202006 HT402006);
%reltime(TPylTX, FT203060 FX403057 FT603057 FT801007 HT002007 HT202007 HT402007);
%reltime(TPlcTX, FT203062 FX403059 FT603059 FT801009 HT002009 HT202009 HT402009);

/*  proc freq; 
tables FT203059 FX403056 FT603056 FT801006 HT002006 HT202006 HT402006/list missing;
run;
proc print data=rel4(obs=15); 
var biground bigtype bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14
FT203059 FX403056 FT603056 FT801006 HT002006 HT202006 HT402006
TParTX12B  TParTX10B  TParTX8B  TParTX6B  TParTX4B  TParTX2B  TParTX0  TParTX2A  TParTX4A  TParTX6A  TParTX8A  TParTX10A  TParTX12A
TParTX12B_ TParTX10B_ TParTX8B_ TParTX6B_ TParTX4B_ TParTX2B_ TParTX0_ TParTX2A_ TParTX4A_ TParTX6A_ TParTX8A_ TParTX10A_ TParTX12A_;
run;
proc means; 
var TParTX0  TParTX2A  TParTX4A  TParTX6A; 
run;   */

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		2. Target report of TARGET's HOSTILITY toward partner during obs. task.
**************************************************************************************/
%reltime(TTcrPX, FT203065 FX403062 FT603062 FT801012 HT002012 HT202012 HT402012);
%reltime(TTanPX, FT203068 FX403065 FT603065 FT801015 HT002015 HT202015 HT402015);
%reltime(TTarPX, FT203070 FX403067 FT603067 FT801017 HT002017 HT202017 HT402017);
%reltime(TTylPX, FT203071 FX403068 FT603068 FT801018 HT002018 HT202018 HT402018);
%reltime(TTlcPX, FT203073 FX403070 FT603070 FT801020 HT002020 HT202020 HT402020);

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		3. Partner report of TARGET's HOSTILITY toward partner during obs. task.
**************************************************************************************/
%reltime(PTcrPX, FZ203068 FZ403065 FZ603062 FZ801012 HZ002012 HZ202012 HZ402012);
%reltime(PTanPX, FZ203071 FZ403068 FZ603065 FZ801015 HZ002015 HZ202015 HZ402015);
%reltime(PTarPX, FZ203073 FZ403070 FZ603067 FZ801017 HZ002017 HZ202017 HZ402017);
%reltime(PTylPX, FZ203074 FZ403071 FZ603068 FZ801018 HZ002018 HZ202018 HZ402018);
%reltime(PTlcPX, FZ203076 FZ403073 FZ603070 FZ801020 HZ002020 HZ202020 HZ402020);

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		4. Partner report of PARTNER's HOSTILITY toward target during obs. task.
**************************************************************************************/
%reltime(PPcrTX, FZ203057 FZ403054 FZ603051 FZ801001 HZ002001 HZ202001 HZ402001);	
%reltime(PPanTX, FZ203060 FZ403057 FZ603054 FZ801004 HZ002004 HZ202004 HZ402004);	
%reltime(PParTX, FZ203062 FZ403059 FZ603056 FZ801006 HZ002006 HZ202006 HZ402006);	
%reltime(PPylTX, FZ203063 FZ403060 FZ603057 FZ801007 HZ002007 HZ202007 HZ402007);	
%reltime(PPlcTX, FZ203065 FZ403062 FZ603059 FZ801009 HZ002009 HZ202009 HZ402009);	

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		5. Target report of PARTNER's SUPPORT toward target during obs. task.
**************************************************************************************/
%reltime(TPwmTX, FT203055 FX403052 FT603052 FT801002 HT002002 HT202002 HT402002);
%reltime(TPcaTX, FT203056 FX403053 FT603053 FT801003 HT002003 HT202003 HT402003);
%reltime(TPunTX, FT203058 FX403055 FT603055 FT801005 HT002005 HT202005 HT402005);
%reltime(TPlsTX, FT203061 FX403058 FT603058 FT801008 HT002008 HT202008 HT402008);
%reltime(TPlaTX, FT203063 FX403060 FT603060 FT801010 HT002010 HT202010 HT402010);
	
/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		6. Target report of TARGET's SUPPORT toward partner during obs. task.
**************************************************************************************/
%reltime(TTwmPX, FT203066 FX403063 FT603063 FT801013 HT002013 HT202013 HT402013);
%reltime(TTcaPX, FT203067 FX403064 FT603064 FT801014 HT002014 HT202014 HT402014);
%reltime(TTunPX, FT203069 FX403066 FT603066 FT801016 HT002016 HT202016 HT402016);
%reltime(TTlsPX, FT203072 FX403069 FT603069 FT801019 HT002019 HT202019 HT402019);
%reltime(TTlaPX, FT203074 FX403071 FT603071 FT801021 HT002021 HT202021 HT402021);

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		7. Partner report of TARGET's SUPPORT toward partner during obs. task.
**************************************************************************************/
%reltime(PTwmPX, FZ203069 FZ403066 FZ603063 FZ801013 HZ002013 HZ202013 HZ402013);
%reltime(PTcaPX, FZ203070 FZ403067 FZ603064 FZ801014 HZ002014 HZ202014 HZ402014);
%reltime(PTunPX, FZ203072 FZ403069 FZ603066 FZ801016 HZ002016 HZ202016 HZ402016);
%reltime(PTlsPX, FZ203075 FZ403072 FZ603069 FZ801019 HZ002019 HZ202019 HZ402019);
%reltime(PTlaPX, FZ203077 FZ403074 FZ603071 FZ801021 HZ002021 HZ202021 HZ402021);

/*************************************************************************************
    VIDEO DISCUSSION QUESTIONNAIRE: 
		8. Partner report of PARTNER's support toward target during obs. task.
**************************************************************************************/
%reltime(PPwmTX, FZ203058 FZ403055 FZ603052 FZ801002 HZ002002 HZ202002 HZ402002);
%reltime(PPcaTX, FZ203059 FZ403056 FZ603053 FZ801003 HZ002003 HZ202003 HZ402003);
%reltime(PPunTX, FZ203061 FZ403058 FZ603055 FZ801005 HZ002005 HZ202005 HZ402005);
%reltime(PPlsTX, FZ203064 FZ403061 FZ603058 FZ801008 HZ002008 HZ202008 HZ402008);
%reltime(PPlaTX, FZ203066 FZ403063 FZ603060 FZ801010 HZ002010 HZ202010 HZ402010);

/*  proc freq; 
tables FZ203064 FZ403061 FZ603058 FZ801008 HZ002008 HZ202008 HZ402008/list missing;
run;
proc print data=rel4(obs=15); 
var biground bigtype bigop2 bigop4 bigop6 bigop8 bigop10 bigop12 bigop14
FZ203064 FZ403061 FZ603058 FZ801008 HZ002008 HZ202008 HZ402008
PPlsTX12B  PPlsTX10B  PPlsTX8B  PPlsTX6B  PPlsTX4B  PPlsTX2B  PPlsTX0  PPlsTX2A  PPlsTX4A  PPlsTX6A  PPlsTX8A  PPlsTX10A  PPlsTX12A
PPlsTX12B_ PPlsTX10B_ PPlsTX8B_ PPlsTX6B_ PPlsTX4B_ PPlsTX2B_ PPlsTX0_ PPlsTX2A_ PPlsTX4A_ PPlsTX6A_ PPlsTX8A_ PPlsTX10A_ PPlsTX12A_;
run;
proc means; 
var PPlsTX0  PPlsTX2A  PPlsTX4A  PPlsTX6A; 
run;   */


/**************************************
	IDENTIFYING PARTNER'S GENDER
***************************************/
%reltime(rel,relstat95 relstat97 relstat99 relstat01 relstat03 relstat05 RELSTAT07); 
format rel12B rel10B rel8B rel6B rel4B rel2B rel0 rel2A rel4A rel6A rel8A rel10A rel12A
rel12B_ rel10B_ rel8B_ rel6B_ rel4B_ rel2B_ rel0_ rel2A_ rel4A_ rel6A_ rel8A_ rel10A_ rel12A_ relstat.; 

array psex7(7) f2opgen f4opgen f6opgen f8opgen f10opgen f12opgen f14opgen; 
do i=1 to 7; 
if psex7(i) in (9,98) then psex7(i)=.; 
end; 
%reltime(psex,f2opgen f4opgen f6opgen f8opgen f10opgen f12opgen f14opgen); 

if GENDER=psex0 then hetero=0; 
else hetero=1; 
label hetero="Couple is not same-sex (time 0)"; 

/*Identifies 3 same-sex Couples, which will be dropped from husband-wife analyses (famid #s: 20, 393, & 406)
famid 335 dating with another female in 1999 & 2007 --> BIGROUND=NO BIG OP, but will also be dropped from analysis for the 'dating' part of dataset 

proc freq; 
tables hetero*gender*psex0/list missing; 
run;
proc print data=rel4; 
where hetero=0; 
var famid gender psex0; 
run;
proc print data=rel4; 
where famid=335;
var famid gender psex2b psex0 psex2a biground relstat95 relstat97 relstat99 relstat01 relstat03 relstat05 relstat07
f2opgen f4opgen f6opgen f8opgen f10opgen f12opgen f14opgen; 
run;*/


/****** PARTNER - AGE AT MARRIAGE******/

format bigopbday mmddyys10.;
OPagemar=round(((bigday-bigopbday)/365.25)/**12*/); 
label OPagemar="OP AGE AT TIME OF MARRIAGE/COHABITATION"; 

/***** DETERMINING AGE AT MARRIAGE FOR HUSBANDS/WIVES  ****/
/** Impute missing values for Partner's sex at time zero (PSEX0) based on Target's sex (GENDER)
  		We will delete same-sex partner later ***/
Format psex0 sex.;

If gender=1 then Prsex0=2;
  else if gender=2 then Prsex0=1;

If gender=1 then HAgeMarT=agemar;
If gender=2 then WAgeMarT=agemar; 
If prsex0=1 then HAgeMarP=opagemar; 
If prsex0=2 then WAgeMarP=opagemar; 

/* proc print data=rel4(obs=25); 
var gender hagemart wagemart;
run;
proc print data=rel4(obs=25); 
var prsex0 hagemarp wagemarp;
run; */ 

/*** Combining data from TARGET-PARTNER to HUSBAND-WIFE --> HAgeMar0, WAgeMar0 ***/

If HAgeMarT ne . then HAgeMar0=HAgeMarT;
  else HAgeMar0=HAgeMarP;
If WAgeMarT ne . then WAgeMar0=WAgeMarT;
  else WAgeMar0=WAgeMarP; 

Label HAgeMar0="HUSBAND AGE at time of FIRST MARRIAGE/COHABITATION";
Label WAgeMar0="WIFE AGE at time of FIRST MARRIAGE/COHABITATION";

run;


/***************************************************************************************
						TRANSLATING TARGET-PARTNER ONTO HUSBAND-WIFE
****************************************************************************************/
data rel5; 
set rel4; 
if hetero; 
if famid ne 335;

%macro husbwife(HHrootW,HWrootH,WHrootW,WWrootH,OHrootW,OWrootH,TrootP,ProotT,reporter); 
/*6 "roots" of new variable names, 2 original variables (PLACE IN CORRECT ORDER!), reporter ("T","P", or "O")*/ 

/*4 original variables (T-->P; P--> T)*/ 
array &TrootP._reltime(13) &TrootP.12B_ &TrootP.10B_ &TrootP.8B_ &TrootP.6B_ &TrootP.4B_ &TrootP.2B_ &TrootP.0_ &TrootP.2A_ &TrootP.4A_ &TrootP.6A_ &TrootP.8A_ &TrootP.10A_ &TrootP.12A_; /*relative to first marriage/cohab*/
array &TrootP._reltime2(13) &TrootP.12B &TrootP.10B &TrootP.8B &TrootP.6B &TrootP.4B &TrootP.2B &TrootP.0 &TrootP.2A &TrootP.4A &TrootP.6A &TrootP.8A &TrootP.10A &TrootP.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
array &ProotT._reltime(13) &ProotT.12B_ &ProotT.10B_ &ProotT.8B_ &ProotT.6B_ &ProotT.4B_ &ProotT.2B_ &ProotT.0_ &ProotT.2A_ &ProotT.4A_ &ProotT.6A_ &ProotT.8A_ &ProotT.10A_ &ProotT.12A_; /*relative to first marriage/cohab*/
array &ProotT._reltime2(13) &ProotT.12B_ &ProotT.10B &ProotT.8B &ProotT.6B &ProotT.4B &ProotT.2B &ProotT.0 &ProotT.2A &ProotT.4A &ProotT.6A &ProotT.8A &ProotT.10A &ProotT.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
/*4 new variables HUSBAND REPORT (H-->W; W--> H)*/ 
array &HHrootW._reltime(13) &HHrootW.12B_ &HHrootW.10B_ &HHrootW.8B_ &HHrootW.6B_ &HHrootW.4B_ &HHrootW.2B_ &HHrootW.0_ &HHrootW.2A_ &HHrootW.4A_ &HHrootW.6A_ &HHrootW.8A_ &HHrootW.10A_ &HHrootW.12A_; /*relative to first marriage/cohab*/
array &HHrootW._reltime2(13) &HHrootW.12B &HHrootW.10B &HHrootW.8B &HHrootW.6B &HHrootW.4B &HHrootW.2B &HHrootW.0 &HHrootW.2A &HHrootW.4A &HHrootW.6A &HHrootW.8A &HHrootW.10A &HHrootW.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
array &HWrootH._reltime(13) &HWrootH.12B_ &HWrootH.10B_ &HWrootH.8B_ &HWrootH.6B_ &HWrootH.4B_ &HWrootH.2B_ &HWrootH.0_ &HWrootH.2A_ &HWrootH.4A_ &HWrootH.6A_ &HWrootH.8A_ &HWrootH.10A_ &HWrootH.12A_; /*relative to first marriage/cohab*/
array &HWrootH._reltime2(13) &HWrootH.12B &HWrootH.10B &HWrootH.8B &HWrootH.6B &HWrootH.4B &HWrootH.2B &HWrootH.0 &HWrootH.2A &HWrootH.4A &HWrootH.6A &HWrootH.8A &HWrootH.10A &HWrootH.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
/*4 new variables WIFE REPORT (H-->W; W--> H)*/ 
array &WHrootW._reltime(13) &WHrootW.12B_ &WHrootW.10B_ &WHrootW.8B_ &WHrootW.6B_ &WHrootW.4B_ &WHrootW.2B_ &WHrootW.0_ &WHrootW.2A_ &WHrootW.4A_ &WHrootW.6A_ &WHrootW.8A_ &WHrootW.10A_ &WHrootW.12A_; /*relative to first marriage/cohab*/
array &WHrootW._reltime2(13) &WHrootW.12B &WHrootW.10B &WHrootW.8B &WHrootW.6B &WHrootW.4B &WHrootW.2B &WHrootW.0 &WHrootW.2A &WHrootW.4A &WHrootW.6A &WHrootW.8A &WHrootW.10A &WHrootW.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
array &WWrootH._reltime(13) &WWrootH.12B_ &WWrootH.10B_ &WWrootH.8B_ &WWrootH.6B_ &WWrootH.4B_ &WWrootH.2B_ &WWrootH.0_ &WWrootH.2A_ &WWrootH.4A_ &WWrootH.6A_ &WWrootH.8A_ &WWrootH.10A_ &WWrootH.12A_; /*relative to first marriage/cohab*/
array &WWrootH._reltime2(13) &Wwrooth.12B &WWrootH.10B &WWrootH.8B &WWrootH.6B &WWrootH.4B &WWrootH.2B &WWrootH.0 &WWrootH.2A &WWrootH.4A &WWrootH.6A &WWrootH.8A &WWrootH.10A &WWrootH.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
/*4 new variables OBSERVER REPORT (H-->W; W--> H)*/
array &OHrootW._reltime(13) &OHrootW.12B_ &OHrootW.10B_ &OHrootW.8B_ &OHrootW.6B_ &OHrootW.4B_ &OHrootW.2B_ &OHrootW.0_ &OHrootW.2A_ &OHrootW.4A_ &OHrootW.6A_ &OHrootW.8A_ &OHrootW.10A_ &OHrootW.12A_; /*relative to first marriage/cohab*/
array &OHrootW._reltime2(13) &OHrootW.12B &OHrootW.10B &OHrootW.8B &OHrootW.6B &OHrootW.4B &OHrootW.2B &OHrootW.0 &OHrootW.2A &OHrootW.4A &OHrootW.6A &OHrootW.8A &OHrootW.10A &OHrootW.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/
array &OWrootH._reltime(13) &OWrootH.12B_ &OWrootH.10B_ &OWrootH.8B_ &OWrootH.6B_ &OWrootH.4B_ &OWrootH.2B_ &OWrootH.0_ &OWrootH.2A_ &OWrootH.4A_ &OWrootH.6A_ &OWrootH.8A_ &OWrootH.10A_ &OWrootH.12A_; /*relative to first marriage/cohab*/
array &OWrootH._reltime2(13) &OWrootH.12B &OWrootH.10B &OWrootH.8B &OWrootH.6B &OWrootH.4B &OWrootH.2B &OWrootH.0 &OWrootH.2A &OWrootH.4A &OWrootH.6A &OWrootH.8A &OWrootH.10A &OWrootH.12A; /*relative to first marriage/cohab, only for rounds in which the OP is first marital/cohab partner*/

/*if target M and reporter target then reporter H
if target M and reporter partner then reporter W
if target F and reporter target then reporter W
if target F and reporter partner then reporter H
regardless of target sex, reporter could be observer O*/ 

/*note: already dropped same-sex couples, so this coding based solely on target's gender*/
if gender=1 /*male*/ then do; 
	if &reporter="T" then do; /*rep=H*/
		do i=1 to 13;
		&HHrootW._reltime(i)=&TrootP._reltime(i); 
		&HHrootW._reltime2(i)=&TrootP._reltime2(i);
		&HWrootH._reltime(i)=&ProotT._reltime(i); 
		&HWrootH._reltime2(i)=&ProotT._reltime2(i); 
		end; end; 
	else if &reporter="P" then do; /*rep=W*/ 
		do i=1 to 13;
		&WHrootW._reltime(i)=&TrootP._reltime(i); 
		&WHrootW._reltime2(i)=&TrootP._reltime2(i);
		&WWrootH._reltime(i)=&ProotT._reltime(i); 
		&WWrootH._reltime2(i)=&ProotT._reltime2(i); 
		end; end; 
	else if &reporter="O" then do; /*rep=O*/ 
		do i=1 to 13;
		&OHrootW._reltime(i)=&TrootP._reltime(i); 
		&OHrootW._reltime2(i)=&TrootP._reltime2(i);
		&OWrootH._reltime(i)=&ProotT._reltime(i); 
		&OWrootH._reltime2(i)=&ProotT._reltime2(i); 
		end; end; 
end; 
else if gender=2 /*female*/ then do; 
	if &reporter="T" then do; /*rep=W*/
		do i=1 to 13;
		&WHrootW._reltime(i)=&ProotT._reltime(i); 
		&WHrootW._reltime2(i)=&ProotT._reltime2(i);
		&WWrootH._reltime(i)=&TrootP._reltime(i); 
		&WWrootH._reltime2(i)=&TrootP._reltime2(i); 
		end; end; 
	else if &reporter="P" then do; /*rep=H*/
		do i=1 to 13;
		&HHrootW._reltime(i)=&ProotT._reltime(i); 
		&HHrootW._reltime2(i)=&ProotT._reltime2(i);
		&HWrootH._reltime(i)=&TrootP._reltime(i); 
		&HWrootH._reltime2(i)=&TrootP._reltime2(i); 
		end; end; 
	else if &reporter="O" then do; /*rep=O*/
		do i=1 to 13;
		&OHrootW._reltime(i)=&ProotT._reltime(i); 
		&OHrootW._reltime2(i)=&ProotT._reltime2(i);
		&OWrootH._reltime(i)=&TrootP._reltime(i); 
		&OWrootH._reltime2(i)=&TrootP._reltime2(i); 
		end; end; 
end; 
%mend; 

/***************************************************************************************
	 @@1. From TARGET report of HOSTILITY --> to HUSBAND's / WIFE's report of HOSTILITY
				(during the past month questionnaire)
****************************************************************************************/
%husbwife(H1HanW,H1WanH,W1HanW,W1WanH,O1HanW,O1WanH,TTanP,TPanT,"T");
/*Number 1 (in the 6 "roots") indicates that these "roots" are originally derived from TARGET report */
/*Note that due to how the macro is written O1HanW and O1WanH would all be missing */

%husbwife(H1HcrW,H1WcrH,W1HcrW,W1WcrH,O1HcrW,O1WcrH,TTcrP,TPcrT,"T");

%husbwife(H1HylW,H1WylH,W1HylW,W1WylH,O1HylW,O1WylH,TTylP,TPylT,"T");
%husbwife(H1HhtW,H1WhtH,W1HhtW,W1WhtH,O1HhtW,O1WhtH,TThtP,TPhtT,"T");
%husbwife(H1HarW,H1WarH,W1HarW,W1WarH,O1HarW,O1WarH,TTarP,TParT,"T");

/*  proc print data=rel5 (obs=25); 
var gender psex0 TTanP0 TPanT0 H1HanW0 H1WanH0 W1HanW0 W1WanH0 O1HanW0 O1WanH0; 
run;  
proc print data=rel5 (obs=25); 
var gender psex0 TTcrP0 TPcrT0 H1HcrW0 H1WcrH0 W1HcrW0 W1WcrH0 O1HcrW0 O1WcrH0; 
run;  */


/***************************************************************************************
	 @@2. From PARTNER report of HOSTILITY --> to HUSBAND's / WIFE's report of HOSTILITY
				(during the past month questionnaire)
****************************************************************************************/
%husbwife(H2HanW,H2WanH,W2HanW,W2WanH,O2HanW,O2WanH,PTanP,PPanT,"P");
/*Number 2 (in the 6 "roots") indicates that these "roots" are originally derived from PARTNER report */
/*Note that due to how the macro is written O2HanW and O2WanH would all be missing */

%husbwife(H2HcrW,H2WcrH,W2HcrW,W2WcrH,O2HcrW,O2WcrH,PTcrP,PPcrT,"P");

%husbwife(H2HylW,H2WylH,W2HylW,W2WylH,O2HylW,O2WylH,PTylP,PPylT,"P");
%husbwife(H2HhtW,H2WhtH,W2HhtW,W2WhtH,O2HhtW,O2WhtH,PThtP,PPhtT,"P");
%husbwife(H2HarW,H2WarH,W2HarW,W2WarH,O2HarW,O2WarH,PTarP,PParT,"P");



/**************************************************************************************************
	 3. From OBSERVER report of T / P HOSTILITY --> to OBSERVER report of H / W HOSTILITY
***************************************************************************************************/
%husbwife(H3HhsW,H3WhsH,W3HhsW,W3WhsH,OHhsW,OWhsH,OThsP,OPhsT,"O");  
Label OHhsW0="Observer - Husband HOSTILITY --> Wife (Time zero)"; 
Label OHhsW2A="Observer - Husband HOSTILITY --> Wife (2 rounds after)"; 
Label OHhsW4A="Observer - Husband HOSTILITY --> Wife (4 rounds after)"; 
Label OHhsW6A="Observer - Husband HOSTILITY --> Wife (6 rounds after)"; 
Label OHhsW8A="Observer - Husband HOSTILITY --> Wife (8 rounds after)"; 

Label OWhsH0="Observer - Wife HOSTILITY --> Husband (Time zero)";
Label OWhsH2A="Observer - Wife HOSTILITY --> Husband (2 rounds after)";
Label OWhsH4A="Observer - Wife HOSTILITY --> Husband (4 rounds after)";
Label OWhsH6A="Observer - Wife HOSTILITY --> Husband (6 rounds after)";
Label OWhsH8A="Observer - Wife HOSTILITY --> Husband (8 rounds after)";

/*number 3 (in the 4 "roots") indicates that these "roots" are derived from OBSERVER report --> these would all be missing! */

/*   proc print data=rel5 (obs=25); 
var gender psex0 OPhsT0 OThsP0 H3HhsW0 H3WhsH0 W3HhsW0 W3WhsH0 OHhsW0 OWhsH0; 
run;  

... See if macro is working with different time points.. 
proc print data=rel5 (obs=25); 
var gender psex0 OPhsT2A OThsP2A  H3HhsW2A  H3WhsH2A  W3HhsW2A  W3WhsH2A  OHhsW2A  OWhsH2A ; 
run;  */


%husbwife(H3HacW,H3WacH,W3HacW,W3WacH,OHacW,OWacH,OTacP,OPacT,"O");
Label OHacW0="Observer - Husband ANGRY COERCION --> Wife (Time zero)"; 
Label OHacW2A="Observer - Husband ANGRY COERCION --> Wife (2 rounds after)"; 
Label OHacW4A="Observer - Husband ANGRY COERCION --> Wife (4 rounds after)"; 
Label OHacW6A="Observer - Husband ANGRY COERCION --> Wife (6 rounds after)"; 
Label OHacW8A="Observer - Husband ANGRY COERCION --> Wife (8 rounds after)"; 

Label OWacH0="Observer - Wife ANGRY COERCION --> Husband (Time zero)";
Label OWacH2A="Observer - Wife ANGRY COERCION --> Husband (2 rounds after)";
Label OWacH4A="Observer - Wife ANGRY COERCION --> Husband (4 rounds after)";
Label OWacH6A="Observer - Wife ANGRY COERCION --> Husband (6 rounds after)";
Label OWacH8A="Observer - Wife ANGRY COERCION --> Husband (8 rounds after)";

%husbwife(H3HehW,H3WehH,W3HehW,W3WehH,OHehW,OWehH,OTehP,OPehT,"O");
Label OHehW0="Observer - Husband ESCALATE HOSTILE --> Wife (Time zero)"; 
Label OHehW2A="Observer - Husband ESCALATE HOSTILE --> Wife (2 rounds after)"; 
Label OHehW4A="Observer - Husband ESCALATE HOSTILE --> Wife (4 rounds after)"; 
Label OHehW6A="Observer - Husband ESCALATE HOSTILE --> Wife (6 rounds after)"; 
Label OHehW8A="Observer - Husband ESCALATE HOSTILE --> Wife (8 rounds after)"; 

Label OWehH0="Observer - Wife ESCALATE HOSTILE --> Husband (Time zero)";
Label OWehH2A="Observer - Wife ESCALATE HOSTILE --> Husband (2 rounds after)";
Label OWehH4A="Observer - Wife ESCALATE HOSTILE --> Husband (4 rounds after)";
Label OWehH6A="Observer - Wife ESCALATE HOSTILE --> Husband (6 rounds after)";
Label OWehH8A="Observer - Wife ESCALATE HOSTILE --> Husband (8 rounds after)";

%husbwife(H3HrhW,H3WrhH,W3HrhW,W3WrhH,OHrhW,OWrhH,OTrhP,OPrhT,"O");
Label OHrhW0="Observer - Husband RECIPROCATE HOSTILE --> Wife (Time zero)"; 
Label OHrhW2A="Observer - Husband RECIPROCATE HOSTILE --> Wife (2 rounds after)"; 
Label OHrhW4A="Observer - Husband RECIPROCATE HOSTILE --> Wife (4 rounds after)"; 
Label OHrhW6A="Observer - Husband RECIPROCATE HOSTILE --> Wife (6 rounds after)"; 
Label OHrhW8A="Observer - Husband RECIPROCATE HOSTILE --> Wife (8 rounds after)"; 

Label OWrhH0="Observer - Wife RECIPROCATE HOSTILE --> Husband (Time zero)";
Label OWrhH2A="Observer - Wife RECIPROCATE HOSTILE --> Husband (2 rounds after)";
Label OWrhH4A="Observer - Wife RECIPROCATE HOSTILE --> Husband (4 rounds after)";
Label OWrhH6A="Observer - Wife RECIPROCATE HOSTILE --> Husband (6 rounds after)";
Label OWrhH8A="Observer - Wife RECIPROCATE HOSTILE --> Husband (8 rounds after)";

%husbwife(H3HanW,H3WanH,W3HanW,W3WanH,OHanW,OWanH,OTanP,OPanT,"O");
Label OHanW0="Observer - Husband ANTISOCIAL --> Wife (Time zero)"; 
Label OHanW2A="Observer - Husband ANTISOCIAL --> Wife (2 rounds after)"; 
Label OHanW4A="Observer - Husband ANTISOCIAL --> Wife (4 rounds after)"; 
Label OHanW6A="Observer - Husband ANTISOCIAL --> Wife (6 rounds after)"; 
Label OHanW8A="Observer - Husband ANTISOCIAL --> Wife (8 rounds after)"; 

Label OWanH0="Observer - Wife ANTISOCIAL --> Husband (Time zero)";
Label OWanH2A="Observer - Wife ANTISOCIAL --> Husband (2 rounds after)";
Label OWanH4A="Observer - Wife ANTISOCIAL --> Husband (4 rounds after)";
Label OWanH6A="Observer - Wife ANTISOCIAL --> Husband (6 rounds after)";
Label OWanH8A="Observer - Wife ANTISOCIAL --> Husband (8 rounds after)";

/***************************************************************************************
	 @@4. From TARGET report of VIDEO DISCUSSION - HOSTILITY 
			---> to HUSBAND's / WIFE's report of VIDEO DISCUSSION - HOSTILITY
****************************************************************************************/
%husbwife(H4HcrWX,H4WcrHX,W4HcrWX,W4WcrHX,O4HcrWX,O4WcrHX,TTcrPX,TPcrTX,"T");
/*Number 4 (in the 6 "roots") indicates that these "roots" are originally derived from TARGET VIDEO DISCUSSION report */
/* O4HcrWX and O4WcrHX would all be missing */

%husbwife(H4HanWX,H4WanHX,W4HanWX,W4WanHX,O4HanWX,O4WanHX,TTanPX,TPanTX,"T");
%husbwife(H4HarWX,H4WarHX,W4HarWX,W4WarHX,O4HarWX,O4WarHX,TTarPX,TParTX,"T");
%husbwife(H4HylWX,H4WylHX,W4HylWX,W4WylHX,O4HylWX,O4WylHX,TTylPX,TPylTX,"T");
%husbwife(H4HlcWX,H4WlcHX,W4HlcWX,W4WlcHX,O4HlcWX,O4WlcHX,TTlcPX,TPlcTX,"T");

/***************************************************************************************
	 @@5. From PARTNER report of VIDEO DISCUSSION - HOSTILITY 
			---> to HUSBAND's / WIFE's report of VIDEO DISCUSSION - HOSTILITY
****************************************************************************************/
/*Number 5 (in the 6 "roots") indicates that these "roots" are originally derived from PARTNER VIDEO DISCUSSION report */
/* O5H_ _WX and O5W_ _HX would all be missing */

%husbwife(H5HcrWX,H5WcrHX,W5HcrWX,W5WcrHX,O5HcrWX,O5WcrHX,PTcrPX,PPcrTX,"P");
%husbwife(H5HanWX,H5WanHX,W5HanWX,W5WanHX,O5HanWX,O5WanHX,PTanPX,PPanTX,"P");
%husbwife(H5HarWX,H5WarHX,W5HarWX,W5WarHX,O5HarWX,O5WarHX,PTarPX,PParTX,"P");
%husbwife(H5HylWX,H5WylHX,W5HylWX,W5WylHX,O5HylWX,O5WylHX,PTylPX,PPylTX,"P");
%husbwife(H5HlcWX,H5WlcHX,W5HlcWX,W5WlcHX,O5HlcWX,O5WlcHX,PTlcPX,PPlcTX,"P");

/***************************************************************************************
	 @@6. From TARGET report of SUPPORT --> to HUSBAND's / WIFE's report of SUPPORT 
				(during the past month questionnaire)
****************************************************************************************/
%husbwife(H6HcaW,H6WcaH,W6HcaW,W6WcaH,O6HcaW,O6WcaH,TTcaP,TPcaT,"T");
/*Number 6 (in the 6 "roots") indicates that these "roots" are originally derived from TARGET report - SUPPORT */
/* O6HcaW and O6WcaH would all be missing */

%husbwife(H6HafW,H6WafH,W6HafW,W6WafH,O6HafW,O6WafH,TTafP,TPafT,"T");
%husbwife(H6HapW,H6WapH,W6HapW,W6WapH,O6HapW,O6WapH,TTapP,TPapT,"T");
%husbwife(H6HhpW,H6WhpH,W6HhpW,W6WhpH,O6HhpW,O6WhpH,TThpP,TPhpT,"T");

/***************************************************************************************
	 @@7. From PARTNER report of SUPPORT --> to HUSBAND's / WIFE's report of SUPPORT
				(during the past month questionnaire)
****************************************************************************************/
%husbwife(H7HcaW,H7WcaH,W7HcaW,W7WcaH,O7HcaW,O7WcaH,PTcaP,PPcaT,"P");
/*Number 7 (in the 6 "roots") indicates that these "roots" are originally derived from PARTNER report - SUPPORT */
/* --> O7HcaW cad O7WcaH would all be missing */

%husbwife(H7HafW,H7WafH,W7HafW,W7WafH,O7HafW,O7WafH,PTafP,PPafT,"P");
%husbwife(H7HapW,H7WapH,W7HapW,W7WapH,O7HapW,O7WapH,PTapP,PPapT,"P");
%husbwife(H7HhpW,H7WhpH,W7HhpW,W7WhpH,O7HhpW,O7WhpH,PThpP,PPhpT,"P");

/*   proc print data=rel5 (obs=25); 
var gender psex0 PTafP0 PPafT0 H7HafW0 H7WafH0 W7HafW0 W7WafH0 O7HafW0 O7WafH0; 
run;  
proc print data=rel5 (obs=25); 
var gender psex0 PThpP0 PPhpT0 H7HhpW0 H7WhpH0 W7HhpW0 W7WhpH0 O7HhpW0 O7WhpH0; 
run;  */

/**************************************************************************************************
	 8. From OBSERVER report of T / P SUPPORT --> to OBSERVER report of H / W SUPPORT
***************************************************************************************************/
/*number 8 (in the 4 "roots") indicates that these "roots" are derived from OBSERVER report --> these would all be missing! */

%husbwife(H8HwmW,H8WwmH,W8HwmW,W8WwmH,OHwmW,OWwmH,OTwmP,OPwmT,"O");  
Label OHwmW0="Observer - Husband WARMTH/SUPPORT --> Wife (Time zero)"; 
Label OHwmW2A="Observer - Husband WARMTH/SUPPORT --> Wife (2 rounds after)"; 
Label OHwmW4A="Observer - Husband WARMTH/SUPPORT --> Wife (4 rounds after)"; 
Label OHwmW6A="Observer - Husband WARMTH/SUPPORT --> Wife (6 rounds after)"; 
Label OHwmW8A="Observer - Husband WARMTH/SUPPORT --> Wife (8 rounds after)"; 

Label OWwmH0="Observer - Wife WARMTH/SUPPORT --> Husband (Time zero)";
Label OWwmH2A="Observer - Wife WARMTH/SUPPORT --> Husband (2 rounds after)";
Label OWwmH4A="Observer - Wife WARMTH/SUPPORT --> Husband (4 rounds after)";
Label OWwmH6A="Observer - Wife WARMTH/SUPPORT --> Husband (6 rounds after)";
Label OWwmH8A="Observer - Wife WARMTH/SUPPORT --> Husband (8 rounds after)";

%husbwife(H8HewW,H8WewH,W8HewW,W8WewH,OHewW,OWewH,OTewP,OPewT,"O");
Label OHewW0="Observer - Husband ESCALATE WARMTH --> Wife (Time zero)"; 
Label OHewW2A="Observer - Husband ESCALATE WARMTH --> Wife (2 rounds after)"; 
Label OHewW4A="Observer - Husband ESCALATE WARMTH --> Wife (4 rounds after)"; 
Label OHewW6A="Observer - Husband ESCALATE WARMTH --> Wife (6 rounds after)"; 
Label OHewW8A="Observer - Husband ESCALATE WARMTH --> Wife (8 rounds after)"; 

Label OWewH0="Observer - Wife ESCALATE WARMTH --> Husband (Time zero)";
Label OWewH2A="Observer - Wife ESCALATE WARMTH --> Husband (2 rounds after)";
Label OWewH4A="Observer - Wife ESCALATE WARMTH --> Husband (4 rounds after)";
Label OWewH6A="Observer - Wife ESCALATE WARMTH --> Husband (6 rounds after)";
Label OWewH8A="Observer - Wife ESCALATE WARMTH --> Husband (8 rounds after)";

%husbwife(H8HrwW,H8WrwH,W8HrwW,W8WrwH,OHrwW,OWrwH,OTrwP,OPrwT,"O");
Label OHrwW0="Observer - Husband RECIPROCATE WARMTH --> Wife (Time zero)"; 
Label OHrwW2A="Observer - Husband RECIPROCATE WARMTH --> Wife (2 rounds after)"; 
Label OHrwW4A="Observer - Husband RECIPROCATE WARMTH --> Wife (4 rounds after)"; 
Label OHrwW6A="Observer - Husband RECIPROCATE WARMTH --> Wife (6 rounds after)"; 
Label OHrwW8A="Observer - Husband RECIPROCATE WARMTH --> Wife (8 rounds after)"; 

Label OWrwH0="Observer - Wife RECIPROCATE WARMTH --> Husband (Time zero)";
Label OWrwH2A="Observer - Wife RECIPROCATE WARMTH --> Husband (2 rounds after)";
Label OWrwH4A="Observer - Wife RECIPROCATE WARMTH --> Husband (4 rounds after)";
Label OWrwH6A="Observer - Wife RECIPROCATE WARMTH --> Husband (6 rounds after)";
Label OWrwH8A="Observer - Wife RECIPROCATE WARMTH --> Husband (8 rounds after)";

%husbwife(H8HarW,H8WarH,W8HarW,W8WarH,OHarW,OWarH,OTarP,OParT,"O");
Label OHarW0="Observer - Husband ASSERTIVENESS --> Wife (Time zero)"; 
Label OHarW2A="Observer - Husband ASSERTIVENESS --> Wife (2 rounds after)"; 
Label OHarW4A="Observer - Husband ASSERTIVENESS --> Wife (4 rounds after)"; 
Label OHarW6A="Observer - Husband ASSERTIVENESS --> Wife (6 rounds after)"; 
Label OHarW8A="Observer - Husband ASSERTIVENESS --> Wife (8 rounds after)"; 

Label OWarH0="Observer - Wife ASSERTIVENESS --> Husband (Time zero)";
Label OWarH2A="Observer - Wife ASSERTIVENESS --> Husband (2 rounds after)";
Label OWarH4A="Observer - Wife ASSERTIVENESS --> Husband (4 rounds after)";
Label OWarH6A="Observer - Wife ASSERTIVENESS --> Husband (6 rounds after)";
Label OWarH8A="Observer - Wife ASSERTIVENESS --> Husband (8 rounds after)";

%husbwife(H8HlrW,H8WlrH,W8HlrW,W8WlrH,OHlrW,OWlrH,OTlrP,OPlrT,"O");
Label OHlrW0="Observer - Husband LISTENER RESPONSIVENESS --> Wife (Time zero)"; 
Label OHlrW2A="Observer - Husband LISTENER RESPONSIVENESS --> Wife (2 rounds after)"; 
Label OHlrW4A="Observer - Husband LISTENER RESPONSIVENESS --> Wife (4 rounds after)"; 
Label OHlrW6A="Observer - Husband LISTENER RESPONSIVENESS --> Wife (6 rounds after)"; 
Label OHlrW8A="Observer - Husband LISTENER RESPONSIVENESS --> Wife (8 rounds after)"; 

Label OWlrH0="Observer - Wife LISTENER RESPONSIVENESS --> Husband (Time zero)";
Label OWlrH2A="Observer - Wife LISTENER RESPONSIVENESS --> Husband (2 rounds after)";
Label OWlrH4A="Observer - Wife LISTENER RESPONSIVENESS --> Husband (4 rounds after)";
Label OWlrH6A="Observer - Wife LISTENER RESPONSIVENESS --> Husband (6 rounds after)";
Label OWlrH8A="Observer - Wife LISTENER RESPONSIVENESS --> Husband (8 rounds after)";

%husbwife(H8HcoW,H8WcoH,W8HcoW,W8WcoH,OHcoW,OWcoH,OTcoP,OPcoT,"O");  
Label OHcoW0="Observer - Husband COMMUNICATION --> Wife (Time zero)"; 
Label OHcoW2A="Observer - Husband COMMUNICATION --> Wife (2 rounds after)"; 
Label OHcoW4A="Observer - Husband COMMUNICATION --> Wife (4 rounds after)"; 
Label OHcoW6A="Observer - Husband COMMUNICATION --> Wife (6 rounds after)"; 
Label OHcoW8A="Observer - Husband COMMUNICATION --> Wife (8 rounds after)"; 

Label OWcoH0="Observer - Wife COMMUNICATION --> Husband (Time zero)";
Label OWcoH2A="Observer - Wife COMMUNICATION --> Husband (2 rounds after)";
Label OWcoH4A="Observer - Wife COMMUNICATION --> Husband (4 rounds after)";
Label OWcoH6A="Observer - Wife COMMUNICATION --> Husband (6 rounds after)";
Label OWcoH8A="Observer - Wife COMMUNICATION --> Husband (8 rounds after)";

%husbwife(H8HprW,H8WprH,W8HprW,W8WprH,OHprW,OWprH,OTprP,OPprT,"O");
Label OHprW0="Observer - Husband PROSOCIAL --> Wife (Time zero)"; 
Label OHprW2A="Observer - Husband PROSOCIAL --> Wife (2 rounds after)"; 
Label OHprW4A="Observer - Husband PROSOCIAL --> Wife (4 rounds after)"; 
Label OHprW6A="Observer - Husband PROSOCIAL --> Wife (6 rounds after)"; 
Label OHprW8A="Observer - Husband PROSOCIAL --> Wife (8 rounds after)"; 

Label OWprH0="Observer - Wife PROSOCIAL --> Husband (Time zero)";
Label OWprH2A="Observer - Wife PROSOCIAL --> Husband (2 rounds after)";
Label OWprH4A="Observer - Wife PROSOCIAL --> Husband (4 rounds after)";
Label OWprH6A="Observer - Wife PROSOCIAL --> Husband (6 rounds after)";
Label OWprH8A="Observer - Wife PROSOCIAL --> Husband (8 rounds after)";

%husbwife(H8HrqW,H8WrqH,W8HrqW,W8WrqH,OHrqW,OWrqH,OTrqP,OPrqT,"O");
Label OHrqW0="Observer -  Husband & Wife REL QUALITY (Time zero)"; 
Label OHrqW2A="Observer - Husband & Wife REL QUALITY (2 rounds after)"; 
Label OHrqW4A="Observer - Husband & Wife REL QUALITY (4 rounds after)"; 
Label OHrqW6A="Observer - Husband & Wife REL QUALITY (6 rounds after)"; 
Label OHrqW8A="Observer - Husband & Wife REL QUALITY (8 rounds after)"; 

Label OWrqH0="Observer - Husband & Wife REL QUALITY (Time zero)";
Label OWrqH2A="Observer - Husband & Wife REL QUALITY (2 rounds after)";
Label OWrqH4A="Observer - Husband & Wife REL QUALITY (4 rounds after)";
Label OWrqH6A="Observer - Husband & Wife REL QUALITY (6 rounds after)";
Label OWrqH8A="Observer - Husband & Wife REL QUALITY (8 rounds after)";

/* proc print data=rel5(obs=25); 
var gender psex0 OPrqT0 OTrqP0 H8HrqW0 H8WrqH0 W8HrqW0 W8WrqH0 OHrqW0 OWrqH0; 
run;  
proc print data=rel5 (obs=25); 
var gender psex0 OPcoT0 OTcoP0 H8HcoW0 H8WcoH0 W8HcoW0 W8WcoH0 OHcoW0 OWcoH0; 
run;  
proc means; 
var OPcoT0 OTcoP0 H8HcoW0 H8WcoH0 W8HcoW0 W8WcoH0 OHcoW0 OWcoH0; 
run;  */

/***************************************************************************************
	 @@9. From TARGET report of VIDEO DISCUSSION - SUPPORT 
			---> to HUSBAND's / WIFE's report of VIDEO DISCUSSION - SUPPORT
****************************************************************************************/
%husbwife(H9HwmWX,H9WwmHX,W9HwmWX,W9WwmHX,O9HwmWX,O9WwmHX,TTwmPX,TPwmTX,"T");
/*Number 9 (in the 6 "roots") indicates that these "roots" are originally derived from TARGET VIDEO DISCUSSION report */
/* O9HwmWX and O9WwmHX would all be missing */

%husbwife(H9HcaWX,H9WcaHX,W9HcaWX,W9WcaHX,O9HcaWX,O9WcaHX,TTcaPX,TPcaTX,"T");
%husbwife(H9HunWX,H9WunHX,W9HunWX,W9WunHX,O9HunWX,O9WunHX,TTunPX,TPunTX,"T");
%husbwife(H9HlsWX,H9WlsHX,W9HlsWX,W9WlsHX,O9HlsWX,O9WlsHX,TTlsPX,TPlsTX,"T");
%husbwife(H9HlaWX,H9WlaHX,W9HlaWX,W9WlaHX,O9HlaWX,O9WlaHX,TTlaPX,TPlaTX,"T");

/***************************************************************************************
	 @@10. From PARTNER report of VIDEO DISCUSSION - SUPPORT 
			---> to HUSBAND's / WIFE's report of VIDEO DISCUSSION - SUPPORT
****************************************************************************************/
/*Number 10 (in the 6 "roots") indicates that these "roots" are originally derived from PARTNER VIDEO DISCUSSION report */
/* O10H_ _WX and O10W_ _HX would all be missing */

%husbwife(H10HwmWX,H10WwmHX,W10HwmWX,W10WwmHX,O10HwmWX,O10WwmHX,PTwmPX,PPwmTX,"P");
%husbwife(H10HcaWX,H10WcaHX,W10HcaWX,W10WcaHX,O10HcaWX,O10WcaHX,PTcaPX,PPcaTX,"P");
%husbwife(H10HunWX,H10WunHX,W10HunWX,W10WunHX,O10HunWX,O10WunHX,PTunPX,PPunTX,"P");
%husbwife(H10HlsWX,H10WlsHX,W10HlsWX,W10WlsHX,O10HlsWX,O10WlsHX,PTlsPX,PPlsTX,"P");
%husbwife(H10HlaWX,H10WlaHX,W10HlaWX,W10WlaHX,O10HlaWX,O10WlaHX,PTlaPX,PPlaTX,"P");

/***************************************************************************************
	 @@11. From TARGET report of TARGET'S RELATIONSHIP INSTABILITY to PARTNER
			---> to HUSBAND's / WIFE's report of RELATIONSHIP INSTABILITY
****************************************************************************************/
/*Number 11 (in the 6 "roots") indicates that these "roots" are originally derived from TARGET REL INST self-report */
/* O11HwmWX and O11WwmHX would all be missing */

%husbwife(H11HseW,H11WseH,W11HseW,W11WseH,O11HseW,O11WseH,TTseP,TPseT,"T");
%husbwife(H11HfrW,H11WfrH,W11HfrW,W11WfrH,O11HfrW,O11WfrH,TTfrP,TPfrT,"T");
%husbwife(H11HtrW,H11WtrH,W11HtrW,W11WtrH,O11HtrW,O11WtrH,TTtrP,TPtrT,"T");
%husbwife(H11HotW,H11WotH,W11HotW,W11WotH,O11HotW,O11WotH,TTotP,TPotT,"T");
%husbwife(H11HthW,H11WthH,W11HthW,W11WthH,O11HthW,O11WthH,TTthP,TPthT,"T");

/***************************************************************************************
	 @@12. From PARTNER report of PARTNER'S RELATIONSHIP INSTABILITY to PARTNER
			---> to HUSBAND's / WIFE's report of RELATIONSHIP INSTABILITY
****************************************************************************************/
/*Number 12 (in the 6 "roots") indicates that these "roots" are originally derived from PARTNER REL INST self-report */
/* O12H_ _WX and O12W_ _HX would all be missing */

%husbwife(H12HseW,H12WseH,W12HseW,W12WseH,O12HseW,O12WseH,PTseP,PPseT,"P");
%husbwife(H12HfrW,H12WfrH,W12HfrW,W12WfrH,O12HfrW,O12WfrH,PTfrP,PPfrT,"P");
%husbwife(H12HtrW,H12WtrH,W12HtrW,W12WtrH,O12HtrW,O12WtrH,PTtrP,PPtrT,"P");
%husbwife(H12HotW,H12WotH,W12HotW,W12WotH,O12HotW,O12WotH,PTotP,PPotT,"P");
%husbwife(H12HthW,H12WthH,W12HthW,W12WthH,O12HthW,O12WthH,PTthP,PPthT,"P");



/******************************************************************************************************
		MACRO "HWComb" IS USED FOR THE TARGET / PARTNER QUESTIONNAIRE ITEMS, 
			both 'during the past month' & video discussion
	--> IT COMBINES THE 1ST H / W REPORT (i.e., H1 or W1, Originally derived from Target) 
				WITH THE 2ND H / W REPORT (i.e., H2 or W2, Originally derived from Partner)
	--> IN OTHER WORDS.., IT COMBINES ABOVE @@1 WITH @@2 (@@6 with @@7), or @@4 WITH @@5 (@@9 with @@10) !
*******************************************************************************************************/

%macro HWComb(combvar,var1,var2);
if &var1 ne . then &combvar=&var1;
else &combvar=&var2;
%mend;

/**************************************************************************************************************
************************************************************************************************************** 
	Husbands' - wives' HOSTILITY 'During the past month' (combines @@1 with @@2)  
	NOTICE that the following work only for Married/Cohabiting couples 
	--> variable ends with (0, 2A, 4A, 6A, 8A), not the one with underscore (i.e., 0_, 2A_, 4A_, 6A_, 8A_ --> any partner)
**************************************************************************************************************
***************************************************************************************************************/  

/*****************
	at TIME ZERO
******************/

/**** Angry - 'during the past month' ****/
%HWComb(HHanW0,H1HanW0,H2HanW0);
%HWComb(HWanH0,H1WanH0,H2WanH0);
%HWComb(WHanW0,W1HanW0,W2HanW0);
%HWComb(WWanH0,W1WanH0,W2WanH0);

label HHanW0="H - How often H get ANGRY at Wife (Time zero)";
label HWanH0="H - How often W get ANGRY at Husband (Time zero)";
label WHanW0="W - How often H get ANGRY at Wife (Time zero)";
label WWanH0="W - How often W get ANGRY at Husband (Time zero)";

/*  proc print data=rel5 (obs=25); 
var  HHanW0 H1HanW0 H2HanW0; 
run;
proc means; 
var HHanW0 H1HanW0 H2HanW0;
run; 
proc print data=rel5 (obs=25); 
var  HWanH0 H1WanH0 H2WanH0; 
run;
proc print data=rel5 (obs=25); 
var  WHanW0 W1HanW0 W2HanW0; 
run;
proc print data=rel5 (obs=25); 
var  WWanH0 W1WanH0 W2WanH0; 
run;  
*/

/**** Criticize - 'during the past month' ****/
%HWComb(HHcrW0,H1HcrW0,H2HcrW0);
%HWComb(HWcrH0,H1WcrH0,H2WcrH0);
%HWComb(WHcrW0,W1HcrW0,W2HcrW0);
%HWComb(WWcrH0,W1WcrH0,W2WcrH0);

label HHcrW0="H - How often H CRITICIZE Wife (Time zero)";
label HWcrH0="H - How often W CRITICIZE Husband (Time zero)";
label WHcrW0="W - How often H CRITICIZE Wife (Time zero)";
label WWcrH0="W - How often W CRITICIZE Husband (Time zero)";

/**** Yell - 'during the past month' ****/
%HWComb(HHylW0,H1HylW0,H2HylW0);
%HWComb(HWylH0,H1WylH0,H2WylH0);
%HWComb(WHylW0,W1HylW0,W2HylW0);
%HWComb(WWylH0,W1WylH0,W2WylH0);

label HHylW0="H - How often H YELL at Wife (Time zero)";
label HWylH0="H - How often W YELL at Husband (Time zero)";
label WHylW0="W - How often H YELL at Wife (Time zero)";
label WWylH0="W - How often W YELL at Husband (Time zero)";

/**** Hit - 'during the past month' ****/
%HWComb(HHhtW0,H1HhtW0,H2HhtW0);
%HWComb(HWhtH0,H1WhtH0,H2WhtH0);
%HWComb(WHhtW0,W1HhtW0,W2HhtW0);
%HWComb(WWhtH0,W1WhtH0,W2WhtH0);

label HHhtW0="H - How often H HIT, push, grab, or shove Wife (Time zero)";
label HWhtH0="H - How often W HIT, push, grab, or shove Husband (Time zero)";
label WHhtW0="W - How often H HIT, push, grab, or shove Wife (Time zero)";
label WWhtH0="W - How often W HIT, push, grab, or shove Husband (Time zero)";

/**** Argue - 'during the past month' ****/
%HWComb(HHarW0,H1HarW0,H2HarW0);
%HWComb(HWarH0,H1WarH0,H2WarH0);
%HWComb(WHarW0,W1HarW0,W2HarW0);
%HWComb(WWarH0,W1WarH0,W2WarH0);

label HHarW0="H - How often H ARGUE with Wife (Time zero)";
label HWarH0="H - How often W ARGUE with Husband (Time zero)";
label WHarW0="W - How often H ARGUE with Wife (Time zero)";
label WWarH0="W - How often W ARGUE with Husband (Time zero)";


/***************************************
	at 2A (2 rounds after Time zero)
****************************************/

/**** Angry - 'during the past month' ****/
%HWComb(HHanW2A,H1HanW2A,H2HanW2A);
%HWComb(HWanH2A,H1WanH2A,H2WanH2A);
%HWComb(WHanW2A,W1HanW2A,W2HanW2A);
%HWComb(WWanH2A,W1WanH2A,W2WanH2A);

label HHanW2A="H - How often H get ANGRY at Wife (2 rounds after)";
label HWanH2A="H - How often W get ANGRY at Husband (2 rounds after)";
label WHanW2A="W - How often H get ANGRY at Wife (2 rounds after)";
label WWanH2A="W - How often W get ANGRY at Husband (2 rounds after)";

/**** Criticize - 'during the past month' ****/
%HWComb(HHcrW2A,H1HcrW2A,H2HcrW2A);
%HWComb(HWcrH2A,H1WcrH2A,H2WcrH2A);
%HWComb(WHcrW2A,W1HcrW2A,W2HcrW2A);
%HWComb(WWcrH2A,W1WcrH2A,W2WcrH2A);

label HHcrW2A="H - How often H CRITICIZE Wife (2 rounds after)";
label HWcrH2A="H - How often W CRITICIZE Husband (2 rounds after)";
label WHcrW2A="W - How often H CRITICIZE Wife (2 rounds after)";
label WWcrH2A="W - How often W CRITICIZE Husband (2 rounds after)";

/**** Yell - 'during the past month' ****/
%HWComb(HHylW2A,H1HylW2A,H2HylW2A);
%HWComb(HWylH2A,H1WylH2A,H2WylH2A);
%HWComb(WHylW2A,W1HylW2A,W2HylW2A);
%HWComb(WWylH2A,W1WylH2A,W2WylH2A);

label HHylW2A="H - How often H YELL at Wife (2 rounds after)";
label HWylH2A="H - How often W YELL at Husband (2 rounds after)";
label WHylW2A="W - How often H YELL at Wife (2 rounds after)";
label WWylH2A="W - How often W YELL at Husband (2 rounds after)";

/**** Hit - 'during the past month' ****/
%HWComb(HHhtW2A,H1HhtW2A,H2HhtW2A);
%HWComb(HWhtH2A,H1WhtH2A,H2WhtH2A);
%HWComb(WHhtW2A,W1HhtW2A,W2HhtW2A);
%HWComb(WWhtH2A,W1WhtH2A,W2WhtH2A);

label HHhtW2A="H - How often H HIT, push, grab, or shove Wife (2 rounds after)";
label HWhtH2A="H - How often W HIT, push, grab, or shove Husband (2 rounds after)";
label WHhtW2A="W - How often H HIT, push, grab, or shove Wife (2 rounds after)";
label WWhtH2A="W - How often W HIT, push, grab, or shove Husband (2 rounds after)";

/**** Argue - 'during the past month' ****/
%HWComb(HHarW2A,H1HarW2A,H2HarW2A);
%HWComb(HWarH2A,H1WarH2A,H2WarH2A);
%HWComb(WHarW2A,W1HarW2A,W2HarW2A);
%HWComb(WWarH2A,W1WarH2A,W2WarH2A);

label HHarW2A="H - How often H ARGUE with Wife (2 rounds after)";
label HWarH2A="H - How often W ARGUE with Husband (2 rounds after)";
label WHarW2A="W - How often H ARGUE with Wife (2 rounds after)";
label WWarH2A="W - How often W ARGUE with Husband (2 rounds after)";


/***************************************
	at 4A (4 rounds after Time zero)
****************************************/

/**** Angry - 'during the past month' ****/
%HWComb(HHanW4A,H1HanW4A,H2HanW4A);
%HWComb(HWanH4A,H1WanH4A,H2WanH4A);
%HWComb(WHanW4A,W1HanW4A,W2HanW4A);
%HWComb(WWanH4A,W1WanH4A,W2WanH4A);

label HHanW4A="H - How often H get ANGRY at Wife (4 rounds after)";
label HWanH4A="H - How often W get ANGRY at Husband (4 rounds after)";
label WHanW4A="W - How often H get ANGRY at Wife (4 rounds after)";
label WWanH4A="W - How often W get ANGRY at Husband (4 rounds after)";

/**** Criticize - 'during the past month' ****/
%HWComb(HHcrW4A,H1HcrW4A,H2HcrW4A);
%HWComb(HWcrH4A,H1WcrH4A,H2WcrH4A);
%HWComb(WHcrW4A,W1HcrW4A,W2HcrW4A);
%HWComb(WWcrH4A,W1WcrH4A,W2WcrH4A);

label HHcrW4A="H - How often H CRITICIZE Wife (4 rounds after)";
label HWcrH4A="H - How often W CRITICIZE Husband (4 rounds after)";
label WHcrW4A="W - How often H CRITICIZE Wife (4 rounds after)";
label WWcrH4A="W - How often W CRITICIZE Husband (4 rounds after)";

/**** Yell - 'during the past month' ****/
%HWComb(HHylW4A,H1HylW4A,H2HylW4A);
%HWComb(HWylH4A,H1WylH4A,H2WylH4A);
%HWComb(WHylW4A,W1HylW4A,W2HylW4A);
%HWComb(WWylH4A,W1WylH4A,W2WylH4A);

label HHylW4A="H - How often H YELL at Wife (4 rounds after)";
label HWylH4A="H - How often W YELL at Husband (4 rounds after)";
label WHylW4A="W - How often H YELL at Wife (4 rounds after)";
label WWylH4A="W - How often W YELL at Husband (4 rounds after)";

/**** Hit - 'during the past month' ****/
%HWComb(HHhtW4A,H1HhtW4A,H2HhtW4A);
%HWComb(HWhtH4A,H1WhtH4A,H2WhtH4A);
%HWComb(WHhtW4A,W1HhtW4A,W2HhtW4A);
%HWComb(WWhtH4A,W1WhtH4A,W2WhtH4A);

label HHhtW4A="H - How often H HIT, push, grab, or shove Wife (4 rounds after)";
label HWhtH4A="H - How often W HIT, push, grab, or shove Husband (4 rounds after)";
label WHhtW4A="W - How often H HIT, push, grab, or shove Wife (4 rounds after)";
label WWhtH4A="W - How often W HIT, push, grab, or shove Husband (4 rounds after)";

/**** Argue - 'during the past month' ****/
%HWComb(HHarW4A,H1HarW4A,H2HarW4A);
%HWComb(HWarH4A,H1WarH4A,H2WarH4A);
%HWComb(WHarW4A,W1HarW4A,W2HarW4A);
%HWComb(WWarH4A,W1WarH4A,W2WarH4A);

label HHarW4A="H - How often H ARGUE with Wife (4 rounds after)";
label HWarH4A="H - How often W ARGUE with Husband (4 rounds after)";
label WHarW4A="W - How often H ARGUE with Wife (4 rounds after)";
label WWarH4A="W - How often W ARGUE with Husband (4 rounds after)";


/***************************************
	at 6A (6 rounds after Time zero)
****************************************/

/**** Angry - 'during the past month' ****/
%HWComb(HHanW6A,H1HanW6A,H2HanW6A);
%HWComb(HWanH6A,H1WanH6A,H2WanH6A);
%HWComb(WHanW6A,W1HanW6A,W2HanW6A);
%HWComb(WWanH6A,W1WanH6A,W2WanH6A);

label HHanW6A="H - How often H get ANGRY at Wife (6 rounds after)";
label HWanH6A="H - How often W get ANGRY at Husband (6 rounds after)";
label WHanW6A="W - How often H get ANGRY at Wife (6 rounds after)";
label WWanH6A="W - How often W get ANGRY at Husband (6 rounds after)";

/**** Criticize - 'during the past month' ****/
%HWComb(HHcrW6A,H1HcrW6A,H2HcrW6A);
%HWComb(HWcrH6A,H1WcrH6A,H2WcrH6A);
%HWComb(WHcrW6A,W1HcrW6A,W2HcrW6A);
%HWComb(WWcrH6A,W1WcrH6A,W2WcrH6A);

label HHcrW6A="H - How often H CRITICIZE Wife (6 rounds after)";
label HWcrH6A="H - How often W CRITICIZE Husband (6 rounds after)";
label WHcrW6A="W - How often H CRITICIZE Wife (6 rounds after)";
label WWcrH6A="W - How often W CRITICIZE Husband (6 rounds after)";

/**** Yell - 'during the past month' ****/
%HWComb(HHylW6A,H1HylW6A,H2HylW6A);
%HWComb(HWylH6A,H1WylH6A,H2WylH6A);
%HWComb(WHylW6A,W1HylW6A,W2HylW6A);
%HWComb(WWylH6A,W1WylH6A,W2WylH6A);

label HHylW6A="H - How often H YELL at Wife (6 rounds after)";
label HWylH6A="H - How often W YELL at Husband (6 rounds after)";
label WHylW6A="W - How often H YELL at Wife (6 rounds after)";
label WWylH6A="W - How often W YELL at Husband (6 rounds after)";

/**** Hit - 'during the past month' ****/
%HWComb(HHhtW6A,H1HhtW6A,H2HhtW6A);
%HWComb(HWhtH6A,H1WhtH6A,H2WhtH6A);
%HWComb(WHhtW6A,W1HhtW6A,W2HhtW6A);
%HWComb(WWhtH6A,W1WhtH6A,W2WhtH6A);

label HHhtW6A="H - How often H HIT, push, grab, or shove Wife (6 rounds after)";
label HWhtH6A="H - How often W HIT, push, grab, or shove Husband (6 rounds after)";
label WHhtW6A="W - How often H HIT, push, grab, or shove Wife (6 rounds after)";
label WWhtH6A="W - How often W HIT, push, grab, or shove Husband (6 rounds after)";

/**** Argue - 'during the past month' ****/
%HWComb(HHarW6A,H1HarW6A,H2HarW6A);
%HWComb(HWarH6A,H1WarH6A,H2WarH6A);
%HWComb(WHarW6A,W1HarW6A,W2HarW6A);
%HWComb(WWarH6A,W1WarH6A,W2WarH6A);

label HHarW6A="H - How often H ARGUE with Wife (6 rounds after)";
label HWarH6A="H - How often W ARGUE with Husband (6 rounds after)";
label WHarW6A="W - How often H ARGUE with Wife (6 rounds after)";
label WWarH6A="W - How often W ARGUE with Husband (6 rounds after)";



/***************************************
	at 8A (8 rounds after Time zero)
****************************************/

/**** Angry - 'during the past month' ****/
%HWComb(HHanW8A,H1HanW8A,H2HanW8A);
%HWComb(HWanH8A,H1WanH8A,H2WanH8A);
%HWComb(WHanW8A,W1HanW8A,W2HanW8A);
%HWComb(WWanH8A,W1WanH8A,W2WanH8A);

label HHanW8A="H - How often H get ANGRY at Wife (8 rounds after)";
label HWanH8A="H - How often W get ANGRY at Husband (8 rounds after)";
label WHanW8A="W - How often H get ANGRY at Wife (8 rounds after)";
label WWanH8A="W - How often W get ANGRY at Husband (8 rounds after)";

/**** Criticize - 'during the past month' ****/
%HWComb(HHcrW8A,H1HcrW8A,H2HcrW8A);
%HWComb(HWcrH8A,H1WcrH8A,H2WcrH8A);
%HWComb(WHcrW8A,W1HcrW8A,W2HcrW8A);
%HWComb(WWcrH8A,W1WcrH8A,W2WcrH8A);

label HHcrW8A="H - How often H CRITICIZE Wife (8 rounds after)";
label HWcrH8A="H - How often W CRITICIZE Husband (8 rounds after)";
label WHcrW8A="W - How often H CRITICIZE Wife (8 rounds after)";
label WWcrH8A="W - How often W CRITICIZE Husband (8 rounds after)";

/**** Yell - 'during the past month' ****/
%HWComb(HHylW8A,H1HylW8A,H2HylW8A);
%HWComb(HWylH8A,H1WylH8A,H2WylH8A);
%HWComb(WHylW8A,W1HylW8A,W2HylW8A);
%HWComb(WWylH8A,W1WylH8A,W2WylH8A);

label HHylW8A="H - How often H YELL at Wife (8 rounds after)";
label HWylH8A="H - How often W YELL at Husband (8 rounds after)";
label WHylW8A="W - How often H YELL at Wife (8 rounds after)";
label WWylH8A="W - How often W YELL at Husband (8 rounds after)";

/**** Hit - 'during the past month' ****/
%HWComb(HHhtW8A,H1HhtW8A,H2HhtW8A);
%HWComb(HWhtH8A,H1WhtH8A,H2WhtH8A);
%HWComb(WHhtW8A,W1HhtW8A,W2HhtW8A);
%HWComb(WWhtH8A,W1WhtH8A,W2WhtH8A);

label HHhtW8A="H - How often H HIT, push, grab, or shove Wife (8 rounds after)";
label HWhtH8A="H - How often W HIT, push, grab, or shove Husband (8 rounds after)";
label WHhtW8A="W - How often H HIT, push, grab, or shove Wife (8 rounds after)";
label WWhtH8A="W - How often W HIT, push, grab, or shove Husband (8 rounds after)";

/**** Argue - 'during the past month' ****/
%HWComb(HHarW8A,H1HarW8A,H2HarW8A);
%HWComb(HWarH8A,H1WarH8A,H2WarH8A);
%HWComb(WHarW8A,W1HarW8A,W2HarW8A);
%HWComb(WWarH8A,W1WarH8A,W2WarH8A);

label HHarW8A="H - How often H ARGUE with Wife (8 rounds after)";
label HWarH8A="H - How often W ARGUE with Husband (8 rounds after)";
label WHarW8A="W - How often H ARGUE with Wife (8 rounds after)";
label WWarH8A="W - How often W ARGUE with Husband (8 rounds after)";

/* proc print data=rel5 (obs=25); 
var WWarH8A W1WarH8A W2WarH8A; 
run; 
proc means;
var WWarH8A W1WarH8A W2WarH8A;
run; 
*/



/************************************************************************************************************** 
************************************************************************************************************** 
	Husbands' - wives' HOSTILITY 'VIDEO discussion Q.' (combines @@4 with @@5)  
	NOTICE that the following work only for Married/Cohabiting couples 
	--> variable ends with (0, 2A, 4A, 6A, 8A), not the one with underscore (i.e., 0_, 2A_, 4A_, 6A_, 8A_ --> any partner)
**************************************************************************************************************
***************************************************************************************************************/  

/*****************
	at TIME ZERO
******************/

/**** Critical - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcrWX0,H4HcrWX0,H5HcrWX0);
%HWComb(HWcrHX0,H4WcrHX0,H5WcrHX0);
%HWComb(WHcrWX0,W4HcrWX0,W5HcrWX0);
%HWComb(WWcrHX0,W4WcrHX0,W5WcrHX0);

label HHcrWX0="H - H was CRITICAL of Wife during discussion (Time zero)";
label HWcrHX0="H - W was CRITICAL of Husband during discussion (Time zero)";
label WHcrWX0="W - H was CRITICAL of Wife during discussion (Time zero)";
label WWcrHX0="W - W was CRITICAL of Husband during discussion (Time zero)";

/**** Got Angry - 'during VIDEO DISCUSSION' ****/
%HWComb(HHanWX0,H4HanWX0,H5HanWX0);
%HWComb(HWanHX0,H4WanHX0,H5WanHX0);
%HWComb(WHanWX0,W4HanWX0,W5HanWX0);
%HWComb(WWanHX0,W4WanHX0,W5WanHX0);

label HHanWX0="H - H got ANGRY with Wife during discussion (Time zero)";
label HWanHX0="H - W got ANGRY with Husband during discussion (Time zero)";
label WHanWX0="W - H got ANGRY with Wife during discussion (Time zero)";
label WWanHX0="W - W got ANGRY with Husband during discussion (Time zero)";

/**** Argued - 'during VIDEO DISCUSSION' ****/
%HWComb(HHarWX0,H4HarWX0,H5HarWX0);
%HWComb(HWarHX0,H4WarHX0,H5WarHX0);
%HWComb(WHarWX0,W4HarWX0,W5HarWX0);
%HWComb(WWarHX0,W4WarHX0,W5WarHX0);

label HHarWX0="H - H ARGUED with Wife during discussion (Time zero)";
label HWarHX0="H - W ARGUED with Husband during discussion (Time zero)";
label WHarWX0="W - H ARGUED with Wife during discussion (Time zero)";
label WWarHX0="W - W ARGUED with Husband during discussion (Time zero)";

/**** Shouted or Yelled - 'during VIDEO DISCUSSION' ****/
%HWComb(HHylWX0,H4HylWX0,H5HylWX0);
%HWComb(HWylHX0,H4WylHX0,H5WylHX0);
%HWComb(WHylWX0,W4HylWX0,W5HylWX0);
%HWComb(WWylHX0,W4WylHX0,W5WylHX0);

label HHylWX0="H - H shouted or YELLED at Wife during discussion (Time zero)";
label HWylHX0="H - W shouted or YELLED at Husband during discussion (Time zero)";
label WHylWX0="W - H shouted or YELLED at Wife during discussion (Time zero)";
label WWylHX0="W - W shouted or YELLED at Husband during discussion (Time zero)";

/**** Lectured or bossed around - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlcWX0,H4HlcWX0,H5HlcWX0);
%HWComb(HWlcHX0,H4WlcHX0,H5WlcHX0);
%HWComb(WHlcWX0,W4HlcWX0,W5HlcWX0);
%HWComb(WWlcHX0,W4WlcHX0,W5WlcHX0);

label HHlcWX0="H - H LECTURED or bossed around Wife during discussion (Time zero)";
label HWlcHX0="H - W LECTURED or bossed around Husband during discussion (Time zero)";
label WHlcWX0="W - H LECTURED or bossed around Wife during discussion (Time zero)";
label WWlcHX0="W - W LECTURED or bossed around Husband during discussion (Time zero)";



/***************************************
	at 2A (2 rounds after Time zero)
****************************************/

/**** Critical - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcrWX2A,H4HcrWX2A,H5HcrWX2A);
%HWComb(HWcrHX2A,H4WcrHX2A,H5WcrHX2A);
%HWComb(WHcrWX2A,W4HcrWX2A,W5HcrWX2A);
%HWComb(WWcrHX2A,W4WcrHX2A,W5WcrHX2A);

label HHcrWX2A="H - H was CRITICAL of Wife during discussion (2 rounds after)";
label HWcrHX2A="H - W was CRITICAL of Husband during discussion (2 rounds after)";
label WHcrWX2A="W - H was CRITICAL of Wife during discussion (2 rounds after)";
label WWcrHX2A="W - W was CRITICAL of Husband during discussion (2 rounds after)";

/**** Got Angry - 'during VIDEO DISCUSSION' ****/
%HWComb(HHanWX2A,H4HanWX2A,H5HanWX2A);
%HWComb(HWanHX2A,H4WanHX2A,H5WanHX2A);
%HWComb(WHanWX2A,W4HanWX2A,W5HanWX2A);
%HWComb(WWanHX2A,W4WanHX2A,W5WanHX2A);

label HHanWX2A="H - H got ANGRY with Wife during discussion (2 rounds after)";
label HWanHX2A="H - W got ANGRY with Husband during discussion (2 rounds after)";
label WHanWX2A="W - H got ANGRY with Wife during discussion (2 rounds after)";
label WWanHX2A="W - W got ANGRY with Husband during discussion (2 rounds after)";

/**** Argued - 'during VIDEO DISCUSSION' ****/
%HWComb(HHarWX2A,H4HarWX2A,H5HarWX2A);
%HWComb(HWarHX2A,H4WarHX2A,H5WarHX2A);
%HWComb(WHarWX2A,W4HarWX2A,W5HarWX2A);
%HWComb(WWarHX2A,W4WarHX2A,W5WarHX2A);

label HHarWX2A="H - H ARGUED with Wife during discussion (2 rounds after)";
label HWarHX2A="H - W ARGUED with Husband during discussion (2 rounds after)";
label WHarWX2A="W - H ARGUED with Wife during discussion (2 rounds after)";
label WWarHX2A="W - W ARGUED with Husband during discussion (2 rounds after)";

/**** Shouted or Yelled - 'during VIDEO DISCUSSION' ****/
%HWComb(HHylWX2A,H4HylWX2A,H5HylWX2A);
%HWComb(HWylHX2A,H4WylHX2A,H5WylHX2A);
%HWComb(WHylWX2A,W4HylWX2A,W5HylWX2A);
%HWComb(WWylHX2A,W4WylHX2A,W5WylHX2A);

label HHylWX2A="H - H shouted or YELLED at Wife during discussion (2 rounds after)";
label HWylHX2A="H - W shouted or YELLED at Husband during discussion (2 rounds after)";
label WHylWX2A="W - H shouted or YELLED at Wife during discussion (2 rounds after)";
label WWylHX2A="W - W shouted or YELLED at Husband during discussion (2 rounds after)";

/**** Lectured or bossed around - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlcWX2A,H4HlcWX2A,H5HlcWX2A);
%HWComb(HWlcHX2A,H4WlcHX2A,H5WlcHX2A);
%HWComb(WHlcWX2A,W4HlcWX2A,W5HlcWX2A);
%HWComb(WWlcHX2A,W4WlcHX2A,W5WlcHX2A);

label HHlcWX2A="H - H LECTURED or bossed around Wife during discussion (2 rounds after)";
label HWlcHX2A="H - W LECTURED or bossed around Husband during discussion (2 rounds after)";
label WHlcWX2A="W - H LECTURED or bossed around Wife during discussion (2 rounds after)";
label WWlcHX2A="W - W LECTURED or bossed around Husband during discussion (2 rounds after)";



/***************************************
	at 4A (4 rounds after)
****************************************/
/**** Critical - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcrWX4A,H4HcrWX4A,H5HcrWX4A);
%HWComb(HWcrHX4A,H4WcrHX4A,H5WcrHX4A);
%HWComb(WHcrWX4A,W4HcrWX4A,W5HcrWX4A);
%HWComb(WWcrHX4A,W4WcrHX4A,W5WcrHX4A);

label HHcrWX4A="H - H was CRITICAL of Wife during discussion (4 rounds after)";
label HWcrHX4A="H - W was CRITICAL of Husband during discussion (4 rounds after)";
label WHcrWX4A="W - H was CRITICAL of Wife during discussion (4 rounds after)";
label WWcrHX4A="W - W was CRITICAL of Husband during discussion (4 rounds after)";

/**** Got Angry - 'during VIDEO DISCUSSION' ****/
%HWComb(HHanWX4A,H4HanWX4A,H5HanWX4A);
%HWComb(HWanHX4A,H4WanHX4A,H5WanHX4A);
%HWComb(WHanWX4A,W4HanWX4A,W5HanWX4A);
%HWComb(WWanHX4A,W4WanHX4A,W5WanHX4A);

label HHanWX4A="H - H got ANGRY with Wife during discussion (4 rounds after)";
label HWanHX4A="H - W got ANGRY with Husband during discussion (4 rounds after)";
label WHanWX4A="W - H got ANGRY with Wife during discussion (4 rounds after)";
label WWanHX4A="W - W got ANGRY with Husband during discussion (4 rounds after)";

/**** Argued - 'during VIDEO DISCUSSION' ****/
%HWComb(HHarWX4A,H4HarWX4A,H5HarWX4A);
%HWComb(HWarHX4A,H4WarHX4A,H5WarHX4A);
%HWComb(WHarWX4A,W4HarWX4A,W5HarWX4A);
%HWComb(WWarHX4A,W4WarHX4A,W5WarHX4A);

label HHarWX4A="H - H ARGUED with Wife during discussion (4 rounds after)";
label HWarHX4A="H - W ARGUED with Husband during discussion (4 rounds after)";
label WHarWX4A="W - H ARGUED with Wife during discussion (4 rounds after)";
label WWarHX4A="W - W ARGUED with Husband during discussion (4 rounds after)";

/**** Shouted or Yelled - 'during VIDEO DISCUSSION' ****/
%HWComb(HHylWX4A,H4HylWX4A,H5HylWX4A);
%HWComb(HWylHX4A,H4WylHX4A,H5WylHX4A);
%HWComb(WHylWX4A,W4HylWX4A,W5HylWX4A);
%HWComb(WWylHX4A,W4WylHX4A,W5WylHX4A);

label HHylWX4A="H - H shouted or YELLED at Wife during discussion (4 rounds after)";
label HWylHX4A="H - W shouted or YELLED at Husband during discussion (4 rounds after)";
label WHylWX4A="W - H shouted or YELLED at Wife during discussion (4 rounds after)";
label WWylHX4A="W - W shouted or YELLED at Husband during discussion (4 rounds after)";

/**** Lectured or bossed around - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlcWX4A,H4HlcWX4A,H5HlcWX4A);
%HWComb(HWlcHX4A,H4WlcHX4A,H5WlcHX4A);
%HWComb(WHlcWX4A,W4HlcWX4A,W5HlcWX4A);
%HWComb(WWlcHX4A,W4WlcHX4A,W5WlcHX4A);

label HHlcWX4A="H - H LECTURED or bossed around Wife during discussion (4 rounds after)";
label HWlcHX4A="H - W LECTURED or bossed around Husband during discussion (4 rounds after)";
label WHlcWX4A="W - H LECTURED or bossed around Wife during discussion (4 rounds after)";
label WWlcHX4A="W - W LECTURED or bossed around Husband during discussion (4 rounds after)";



/***************************************
	at 6A (6 rounds after)
****************************************/
/**** Critical - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcrWX6A,H4HcrWX6A,H5HcrWX6A);
%HWComb(HWcrHX6A,H4WcrHX6A,H5WcrHX6A);
%HWComb(WHcrWX6A,W4HcrWX6A,W5HcrWX6A);
%HWComb(WWcrHX6A,W4WcrHX6A,W5WcrHX6A);

label HHcrWX6A="H - H was CRITICAL of Wife during discussion (6 rounds after)";
label HWcrHX6A="H - W was CRITICAL of Husband during discussion (6 rounds after)";
label WHcrWX6A="W - H was CRITICAL of Wife during discussion (6 rounds after)";
label WWcrHX6A="W - W was CRITICAL of Husband during discussion (6 rounds after)";

/**** Got Angry - 'during VIDEO DISCUSSION' ****/
%HWComb(HHanWX6A,H4HanWX6A,H5HanWX6A);
%HWComb(HWanHX6A,H4WanHX6A,H5WanHX6A);
%HWComb(WHanWX6A,W4HanWX6A,W5HanWX6A);
%HWComb(WWanHX6A,W4WanHX6A,W5WanHX6A);

label HHanWX6A="H - H got ANGRY with Wife during discussion (6 rounds after)";
label HWanHX6A="H - W got ANGRY with Husband during discussion (6 rounds after)";
label WHanWX6A="W - H got ANGRY with Wife during discussion (6 rounds after)";
label WWanHX6A="W - W got ANGRY with Husband during discussion (6 rounds after)";

/**** Argued - 'during VIDEO DISCUSSION' ****/
%HWComb(HHarWX6A,H4HarWX6A,H5HarWX6A);
%HWComb(HWarHX6A,H4WarHX6A,H5WarHX6A);
%HWComb(WHarWX6A,W4HarWX6A,W5HarWX6A);
%HWComb(WWarHX6A,W4WarHX6A,W5WarHX6A);

label HHarWX6A="H - H ARGUED with Wife during discussion (6 rounds after)";
label HWarHX6A="H - W ARGUED with Husband during discussion (6 rounds after)";
label WHarWX6A="W - H ARGUED with Wife during discussion (6 rounds after)";
label WWarHX6A="W - W ARGUED with Husband during discussion (6 rounds after)";

/**** Shouted or Yelled - 'during VIDEO DISCUSSION' ****/
%HWComb(HHylWX6A,H4HylWX6A,H5HylWX6A);
%HWComb(HWylHX6A,H4WylHX6A,H5WylHX6A);
%HWComb(WHylWX6A,W4HylWX6A,W5HylWX6A);
%HWComb(WWylHX6A,W4WylHX6A,W5WylHX6A);

label HHylWX6A="H - H shouted or YELLED at Wife during discussion (6 rounds after)";
label HWylHX6A="H - W shouted or YELLED at Husband during discussion (6 rounds after)";
label WHylWX6A="W - H shouted or YELLED at Wife during discussion (6 rounds after)";
label WWylHX6A="W - W shouted or YELLED at Husband during discussion (6 rounds after)";

/**** Lectured or bossed around - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlcWX6A,H4HlcWX6A,H5HlcWX6A);
%HWComb(HWlcHX6A,H4WlcHX6A,H5WlcHX6A);
%HWComb(WHlcWX6A,W4HlcWX6A,W5HlcWX6A);
%HWComb(WWlcHX6A,W4WlcHX6A,W5WlcHX6A);

label HHlcWX6A="H - H LECTURED or bossed around Wife during discussion (6 rounds after)";
label HWlcHX6A="H - W LECTURED or bossed around Husband during discussion (6 rounds after)";
label WHlcWX6A="W - H LECTURED or bossed around Wife during discussion (6 rounds after)";
label WWlcHX6A="W - W LECTURED or bossed around Husband during discussion (6 rounds after)";



/***************************************
	at 8A (8 rounds after)
****************************************/
/**** Critical - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcrWX8A,H4HcrWX8A,H5HcrWX8A);
%HWComb(HWcrHX8A,H4WcrHX8A,H5WcrHX8A);
%HWComb(WHcrWX8A,W4HcrWX8A,W5HcrWX8A);
%HWComb(WWcrHX8A,W4WcrHX8A,W5WcrHX8A);

label HHcrWX8A="H - H was CRITICAL of Wife during discussion (8 rounds after)";
label HWcrHX8A="H - W was CRITICAL of Husband during discussion (8 rounds after)";
label WHcrWX8A="W - H was CRITICAL of Wife during discussion (8 rounds after)";
label WWcrHX8A="W - W was CRITICAL of Husband during discussion (8 rounds after)";

/**** Got Angry - 'during VIDEO DISCUSSION' ****/
%HWComb(HHanWX8A,H4HanWX8A,H5HanWX8A);
%HWComb(HWanHX8A,H4WanHX8A,H5WanHX8A);
%HWComb(WHanWX8A,W4HanWX8A,W5HanWX8A);
%HWComb(WWanHX8A,W4WanHX8A,W5WanHX8A);

label HHanWX8A="H - H got ANGRY with Wife during discussion (8 rounds after)";
label HWanHX8A="H - W got ANGRY with Husband during discussion (8 rounds after)";
label WHanWX8A="W - H got ANGRY with Wife during discussion (8 rounds after)";
label WWanHX8A="W - W got ANGRY with Husband during discussion (8 rounds after)";

/**** Argued - 'during VIDEO DISCUSSION' ****/
%HWComb(HHarWX8A,H4HarWX8A,H5HarWX8A);
%HWComb(HWarHX8A,H4WarHX8A,H5WarHX8A);
%HWComb(WHarWX8A,W4HarWX8A,W5HarWX8A);
%HWComb(WWarHX8A,W4WarHX8A,W5WarHX8A);

label HHarWX8A="H - H ARGUED with Wife during discussion (8 rounds after)";
label HWarHX8A="H - W ARGUED with Husband during discussion (8 rounds after)";
label WHarWX8A="W - H ARGUED with Wife during discussion (8 rounds after)";
label WWarHX8A="W - W ARGUED with Husband during discussion (8 rounds after)";

/**** Shouted or Yelled - 'during VIDEO DISCUSSION' ****/
%HWComb(HHylWX8A,H4HylWX8A,H5HylWX8A);
%HWComb(HWylHX8A,H4WylHX8A,H5WylHX8A);
%HWComb(WHylWX8A,W4HylWX8A,W5HylWX8A);
%HWComb(WWylHX8A,W4WylHX8A,W5WylHX8A);

label HHylWX8A="H - H shouted or YELLED at Wife during discussion (8 rounds after)";
label HWylHX8A="H - W shouted or YELLED at Husband during discussion (8 rounds after)";
label WHylWX8A="W - H shouted or YELLED at Wife during discussion (8 rounds after)";
label WWylHX8A="W - W shouted or YELLED at Husband during discussion (8 rounds after)";

/**** Lectured or bossed around - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlcWX8A,H4HlcWX8A,H5HlcWX8A);
%HWComb(HWlcHX8A,H4WlcHX8A,H5WlcHX8A);
%HWComb(WHlcWX8A,W4HlcWX8A,W5HlcWX8A);
%HWComb(WWlcHX8A,W4WlcHX8A,W5WlcHX8A);

label HHlcWX8A="H - H LECTURED or bossed around Wife during discussion (8 rounds after)";
label HWlcHX8A="H - W LECTURED or bossed around Husband during discussion (8 rounds after)";
label WHlcWX8A="W - H LECTURED or bossed around Wife during discussion (8 rounds after)";
label WWlcHX8A="W - W LECTURED or bossed around Husband during discussion (8 rounds after)";

/* proc print data=rel5(obs=25); 
var WHlcWX8A W4HlcWX8A W5HlcWX8A; 
run;  */


/**************************************************************************************************************  
**************************************************************************************************************
	Husbands' - wives' SUPPORT 'During the past month' (combines @@6 with @@7)  
	NOTICE that the following work only for Married/Cohabiting couples 
	--> variable ends with (0, 2A, 4A, 6A, 8A), not the one with underscore (i.e., 0_, 2A_, 4A_, 6A_, 8A_ --> any partner)
**************************************************************************************************************
***************************************************************************************************************/  

/*****************
	at TIME ZERO
******************/

/**** Care - 'during the past month' ****/
%HWComb(HHcaW0,H6HcaW0,H7HcaW0);
%HWComb(HWcaH0,H6WcaH0,H7WcaH0);
%HWComb(WHcaW0,W6HcaW0,W7HcaW0);
%HWComb(WWcaH0,W6WcaH0,W7WcaH0);

label HHcaW0="H - How often H let Wife know he really CARES about W (Time zero)";
label HWcaH0="H - How often W let Husband know she really CARES about H (Time zero)";
label WHcaW0="W - How often H let Wife know he really CARES about W (Time zero)";
label WWcaH0="W - How often W let Husband know she really CARES about H (Time zero)";

/**** Affectionate - 'during the past month' ****/
%HWComb(HHafW0,H6HafW0,H7HafW0);
%HWComb(HWafH0,H6WafH0,H7WafH0);
%HWComb(WHafW0,W6HafW0,W7HafW0);
%HWComb(WWafH0,W6WafH0,W7WafH0);

label HHafW0="H - How often H act loving & AFFECTIONATE toward Wife (Time zero)";
label HWafH0="H - How often W act loving & AFFECTIONATE toward Husband (Time zero)";
label WHafW0="W - How often H act loving & AFFECTIONATE toward Wife (Time zero)";
label WWafH0="W - How often W act loving & AFFECTIONATE toward Husband (Time zero)";

/**** Appreciates - 'during the past month' ****/
%HWComb(HHapW0,H6HapW0,H7HapW0);
%HWComb(HWapH0,H6WapH0,H7WapH0);
%HWComb(WHapW0,W6HapW0,W7HapW0);
%HWComb(WWapH0,W6WapH0,W7WapH0);

label HHapW0="H - How often H APPRECIATES Wife (Time zero)";
label HWapH0="H - How often W APPRECIATES Husband (Time zero)";
label WHapW0="W - How often H APPRECIATES Wife (Time zero)";
label WWapH0="W - How often W APPRECIATES Husband (Time zero)";

/**** Help - 'during the past month' ****/
%HWComb(HHhpW0,H6HhpW0,H7HhpW0);
%HWComb(HWhpH0,H6WhpH0,H7WhpH0);
%HWComb(WHhpW0,W6HhpW0,W7HhpW0);
%HWComb(WWhpH0,W6WhpH0,W7WhpH0);

label HHhpW0="H - How often H HELP Wife (Time zero)";
label HWhpH0="H - How often W HELP Husband (Time zero)";
label WHhpW0="W - How often H HELP Wife (Time zero)";
label WWhpH0="W - How often W HELP Husband (Time zero)";



/***************************************
	at 2A (2 rounds after Time zero)
****************************************/
/**** Care - 'during the past month' ****/
%HWComb(HHcaW2A,H6HcaW2A,H7HcaW2A);
%HWComb(HWcaH2A,H6WcaH2A,H7WcaH2A);
%HWComb(WHcaW2A,W6HcaW2A,W7HcaW2A);
%HWComb(WWcaH2A,W6WcaH2A,W7WcaH2A);

label HHcaW2A="H - How often H let Wife know he really CARES about W (2 rounds after)";
label HWcaH2A="H - How often W let Husband know she really CARES about H (2 rounds after)";
label WHcaW2A="W - How often H let Wife know he really CARES about W (2 rounds after)";
label WWcaH2A="W - How often W let Husband know she really CARES about H (2 rounds after)";

/**** Affectionate - 'during the past month' ****/
%HWComb(HHafW2A,H6HafW2A,H7HafW2A);
%HWComb(HWafH2A,H6WafH2A,H7WafH2A);
%HWComb(WHafW2A,W6HafW2A,W7HafW2A);
%HWComb(WWafH2A,W6WafH2A,W7WafH2A);

label HHafW2A="H - How often H act loving & AFFECTIONATE toward Wife (2 rounds after)";
label HWafH2A="H - How often W act loving & AFFECTIONATE toward Husband (2 rounds after)";
label WHafW2A="W - How often H act loving & AFFECTIONATE toward Wife (2 rounds after)";
label WWafH2A="W - How often W act loving & AFFECTIONATE toward Husband (2 rounds after)";

/**** Appreciates - 'during the past month' ****/
%HWComb(HHapW2A,H6HapW2A,H7HapW2A);
%HWComb(HWapH2A,H6WapH2A,H7WapH2A);
%HWComb(WHapW2A,W6HapW2A,W7HapW2A);
%HWComb(WWapH2A,W6WapH2A,W7WapH2A);

label HHapW2A="H - How often H APPRECIATES Wife (2 rounds after)";
label HWapH2A="H - How often W APPRECIATES Husband (2 rounds after)";
label WHapW2A="W - How often H APPRECIATES Wife (2 rounds after)";
label WWapH2A="W - How often W APPRECIATES Husband (2 rounds after)";

/**** Help - 'during the past month' ****/
%HWComb(HHhpW2A,H6HhpW2A,H7HhpW2A);
%HWComb(HWhpH2A,H6WhpH2A,H7WhpH2A);
%HWComb(WHhpW2A,W6HhpW2A,W7HhpW2A);
%HWComb(WWhpH2A,W6WhpH2A,W7WhpH2A);

label HHhpW2A="H - How often H HELP Wife (2 rounds after)";
label HWhpH2A="H - How often W HELP Husband (2 rounds after)";
label WHhpW2A="W - How often H HELP Wife (2 rounds after)";
label WWhpH2A="W - How often W HELP Husband (2 rounds after)";

/*  proc print data=rel5 (obs=25); 
var HHhpW2A H6HhpW2A H7HhpW2A;  
run;
proc means; 
var HHhpW2A H6HhpW2A H7HhpW2A;  
run; */

/***************************************
	at 4A (4 rounds after)
****************************************/
/**** Care - 'during the past month' ****/
%HWComb(HHcaW4A,H6HcaW4A,H7HcaW4A);
%HWComb(HWcaH4A,H6WcaH4A,H7WcaH4A);
%HWComb(WHcaW4A,W6HcaW4A,W7HcaW4A);
%HWComb(WWcaH4A,W6WcaH4A,W7WcaH4A);

label HHcaW4A="H - How often H let Wife know he really CARES about W (4 rounds after)";
label HWcaH4A="H - How often W let Husband know she really CARES about H (4 rounds after)";
label WHcaW4A="W - How often H let Wife know he really CARES about W (4 rounds after)";
label WWcaH4A="W - How often W let Husband know she really CARES about H (4 rounds after)";

/**** Affectionate - 'during the past month' ****/
%HWComb(HHafW4A,H6HafW4A,H7HafW4A);
%HWComb(HWafH4A,H6WafH4A,H7WafH4A);
%HWComb(WHafW4A,W6HafW4A,W7HafW4A);
%HWComb(WWafH4A,W6WafH4A,W7WafH4A);

label HHafW4A="H - How often H act loving & AFFECTIONATE toward Wife (4 rounds after)";
label HWafH4A="H - How often W act loving & AFFECTIONATE toward Husband (4 rounds after)";
label WHafW4A="W - How often H act loving & AFFECTIONATE toward Wife (4 rounds after)";
label WWafH4A="W - How often W act loving & AFFECTIONATE toward Husband (4 rounds after)";

/**** Appreciates - 'during the past month' ****/
%HWComb(HHapW4A,H6HapW4A,H7HapW4A);
%HWComb(HWapH4A,H6WapH4A,H7WapH4A);
%HWComb(WHapW4A,W6HapW4A,W7HapW4A);
%HWComb(WWapH4A,W6WapH4A,W7WapH4A);

label HHapW4A="H - How often H APPRECIATES Wife (4 rounds after)";
label HWapH4A="H - How often W APPRECIATES Husband (4 rounds after)";
label WHapW4A="W - How often H APPRECIATES Wife (4 rounds after)";
label WWapH4A="W - How often W APPRECIATES Husband (4 rounds after)";

/**** Help - 'during the past month' ****/
%HWComb(HHhpW4A,H6HhpW4A,H7HhpW4A);
%HWComb(HWhpH4A,H6WhpH4A,H7WhpH4A);
%HWComb(WHhpW4A,W6HhpW4A,W7HhpW4A);
%HWComb(WWhpH4A,W6WhpH4A,W7WhpH4A);

label HHhpW4A="H - How often H HELP Wife (4 rounds after)";
label HWhpH4A="H - How often W HELP Husband (4 rounds after)";
label WHhpW4A="W - How often H HELP Wife (4 rounds after)";
label WWhpH4A="W - How often W HELP Husband (4 rounds after)";


/***************************************
	at 6A (6 rounds after)
****************************************/

/**** Care - 'during the past month' ****/
%HWComb(HHcaW6A,H6HcaW6A,H7HcaW6A);
%HWComb(HWcaH6A,H6WcaH6A,H7WcaH6A);
%HWComb(WHcaW6A,W6HcaW6A,W7HcaW6A);
%HWComb(WWcaH6A,W6WcaH6A,W7WcaH6A);

label HHcaW6A="H - How often H let Wife know he really CARES about W (6 rounds after)";
label HWcaH6A="H - How often W let Husband know she really CARES about H (6 rounds after)";
label WHcaW6A="W - How often H let Wife know he really CARES about W (6 rounds after)";
label WWcaH6A="W - How often W let Husband know she really CARES about H (6 rounds after)";

/**** Affectionate - 'during the past month' ****/
%HWComb(HHafW6A,H6HafW6A,H7HafW6A);
%HWComb(HWafH6A,H6WafH6A,H7WafH6A);
%HWComb(WHafW6A,W6HafW6A,W7HafW6A);
%HWComb(WWafH6A,W6WafH6A,W7WafH6A);

label HHafW6A="H - How often H act loving & AFFECTIONATE toward Wife (6 rounds after)";
label HWafH6A="H - How often W act loving & AFFECTIONATE toward Husband (6 rounds after)";
label WHafW6A="W - How often H act loving & AFFECTIONATE toward Wife (6 rounds after)";
label WWafH6A="W - How often W act loving & AFFECTIONATE toward Husband (6 rounds after)";

/**** Appreciates - 'during the past month' ****/
%HWComb(HHapW6A,H6HapW6A,H7HapW6A);
%HWComb(HWapH6A,H6WapH6A,H7WapH6A);
%HWComb(WHapW6A,W6HapW6A,W7HapW6A);
%HWComb(WWapH6A,W6WapH6A,W7WapH6A);

label HHapW6A="H - How often H APPRECIATES Wife (6 rounds after)";
label HWapH6A="H - How often W APPRECIATES Husband (6 rounds after)";
label WHapW6A="W - How often H APPRECIATES Wife (6 rounds after)";
label WWapH6A="W - How often W APPRECIATES Husband (6 rounds after)";

/**** Help - 'during the past month' ****/
%HWComb(HHhpW6A,H6HhpW6A,H7HhpW6A);
%HWComb(HWhpH6A,H6WhpH6A,H7WhpH6A);
%HWComb(WHhpW6A,W6HhpW6A,W7HhpW6A);
%HWComb(WWhpH6A,W6WhpH6A,W7WhpH6A);

label HHhpW6A="H - How often H HELP Wife (6 rounds after)";
label HWhpH6A="H - How often W HELP Husband (6 rounds after)";
label WHhpW6A="W - How often H HELP Wife (6 rounds after)";
label WWhpH6A="W - How often W HELP Husband (6 rounds after)";



/***************************************
	at 8A (8 rounds after)
****************************************/

/**** Care - 'during the past month' ****/
%HWComb(HHcaW8A,H6HcaW8A,H7HcaW8A);
%HWComb(HWcaH8A,H6WcaH8A,H7WcaH8A);
%HWComb(WHcaW8A,W6HcaW8A,W7HcaW8A);
%HWComb(WWcaH8A,W6WcaH8A,W7WcaH8A);

label HHcaW8A="H - How often H let Wife know he really CARES about W (8 rounds after)";
label HWcaH8A="H - How often W let Husband know she really CARES about H (8 rounds after)";
label WHcaW8A="W - How often H let Wife know he really CARES about W (8 rounds after)";
label WWcaH8A="W - How often W let Husband know she really CARES about H (8 rounds after)";

/**** Affectionate - 'during the past month' ****/
%HWComb(HHafW8A,H6HafW8A,H7HafW8A);
%HWComb(HWafH8A,H6WafH8A,H7WafH8A);
%HWComb(WHafW8A,W6HafW8A,W7HafW8A);
%HWComb(WWafH8A,W6WafH8A,W7WafH8A);

label HHafW8A="H - How often H act loving & AFFECTIONATE toward Wife (8 rounds after)";
label HWafH8A="H - How often W act loving & AFFECTIONATE toward Husband (8 rounds after)";
label WHafW8A="W - How often H act loving & AFFECTIONATE toward Wife (8 rounds after)";
label WWafH8A="W - How often W act loving & AFFECTIONATE toward Husband (8 rounds after)";

/**** Appreciates - 'during the past month' ****/
%HWComb(HHapW8A,H6HapW8A,H7HapW8A);
%HWComb(HWapH8A,H6WapH8A,H7WapH8A);
%HWComb(WHapW8A,W6HapW8A,W7HapW8A);
%HWComb(WWapH8A,W6WapH8A,W7WapH8A);

label HHapW8A="H - How often H APPRECIATES Wife (8 rounds after)";
label HWapH8A="H - How often W APPRECIATES Husband (8 rounds after)";
label WHapW8A="W - How often H APPRECIATES Wife (8 rounds after)";
label WWapH8A="W - How often W APPRECIATES Husband (8 rounds after)";

/**** Help - 'during the past month' ****/
%HWComb(HHhpW8A,H6HhpW8A,H7HhpW8A);
%HWComb(HWhpH8A,H6WhpH8A,H7WhpH8A);
%HWComb(WHhpW8A,W6HhpW8A,W7HhpW8A);
%HWComb(WWhpH8A,W6WhpH8A,W7WhpH8A);

label HHhpW8A="H - How often H HELP Wife (8 rounds after)";
label HWhpH8A="H - How often W HELP Husband (8 rounds after)";
label WHhpW8A="W - How often H HELP Wife (8 rounds after)";
label WWhpH8A="W - How often W HELP Husband (8 rounds after)";



/**************************************************************************************************************  
	Husbands' - wives' SUPPORT 'VIDEO discussion Q.' (combines @@9 with @@10)  
	NOTICE that the following work only for Married/Cohabiting couples 
	--> variable ends with (0, 2A, 4A, 6A, 8A), not the one with underscore (i.e., 0_, 2A_, 4A_, 6A_, 8A_ --> any partner)
***************************************************************************************************************/  

/*****************
	at TIME ZERO
******************/

/**** Warm - 'during VIDEO DISCUSSION' ****/
%HWComb(HHwmWX0,H9HwmWX0,H10HwmWX0);
%HWComb(HWwmHX0,H9WwmHX0,H10WwmHX0);
%HWComb(WHwmWX0,W9HwmWX0,W10HwmWX0);
%HWComb(WWwmHX0,W9WwmHX0,W10WwmHX0);

label HHwmWX0="H - H was WARM / affectionate toward W - during discussion (Time zero)";
label HWwmHX0="H - W was WARM / affectionate toward H - during discussion (Time zero)";
label WHwmWX0="W - H was WARM / affectionate toward W - during discussion (Time zero)";
label WWwmHX0="W - W was WARM / affectionate toward H - during discussion (Time zero)";

/**** Care - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcaWX0,H9HcaWX0,H10HcaWX0);
%HWComb(HWcaHX0,H9WcaHX0,H10WcaHX0);
%HWComb(WHcaWX0,W9HcaWX0,W10HcaWX0);
%HWComb(WWcaHX0,W9WcaHX0,W10WcaHX0);

label HHcaWX0="H - H CARED about W opinions / feelings - during discussion (Time zero)";
label HWcaHX0="H - W CARED about H opinions / feelings - during discussion (Time zero)";
label WHcaWX0="W - H CARED about W opinions / feelings - during discussion (Time zero)";
label WWcaHX0="W - W CARED about H opinions / feelings - during discussion (Time zero)";

/**** Understand - 'during VIDEO DISCUSSION' ****/
%HWComb(HHunWX0,H9HunWX0,H10HunWX0);
%HWComb(HWunHX0,H9WunHX0,H10WunHX0);
%HWComb(WHunWX0,W9HunWX0,W10HunWX0);
%HWComb(WWunHX0,W9WunHX0,W10WunHX0);

label HHunWX0="H - H tried to UNDERSTAND W - during discussion (Time zero)";
label HWunHX0="H - W tried to UNDERSTAND H - during discussion (Time zero)";
label WHunWX0="W - H tried to UNDERSTAND W - during discussion (Time zero)";
label WWunHX0="W - W tried to UNDERSTAND H - during discussion (Time zero)";

/**** Listened - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlsWX0,H9HlsWX0,H10HlsWX0);
%HWComb(HWlsHX0,H9WlsHX0,H10WlsHX0);
%HWComb(WHlsWX0,W9HlsWX0,W10HlsWX0);
%HWComb(WWlsHX0,W9WlsHX0,W10WlsHX0);

label HHlsWX0="H - H LISTENED to W - during discussion (Time zero)";
label HWlsHX0="H - W LISTENED to H - during discussion (Time zero)";
label WHlsWX0="W - H LISTENED to W - during discussion (Time zero)";
label WWlsHX0="W - W LISTENED to H - during discussion (Time zero)";

/**** Laugh - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlaWX0,H9HlaWX0,H10HlaWX0);
%HWComb(HWlaHX0,H9WlaHX0,H10WlaHX0);
%HWComb(WHlaWX0,W9HlaWX0,W10HlaWX0);
%HWComb(WWlaHX0,W9WlaHX0,W10WlaHX0);

label HHlaWX0="H - H made W LAUGH and feel good - during discussion (Time zero)";
label HWlaHX0="H - W made H LAUGH and feel good - during discussion (Time zero)";
label WHlaWX0="W - H made W LAUGH and feel good - during discussion (Time zero)";
label WWlaHX0="W - W made H LAUGH and feel good - during discussion (Time zero)";


/***************************************
	at 2A (2 rounds after)
****************************************/
/**** Warm - 'during VIDEO DISCUSSION' ****/
%HWComb(HHwmWX2A,H9HwmWX2A,H10HwmWX2A);
%HWComb(HWwmHX2A,H9WwmHX2A,H10WwmHX2A);
%HWComb(WHwmWX2A,W9HwmWX2A,W10HwmWX2A);
%HWComb(WWwmHX2A,W9WwmHX2A,W10WwmHX2A);

label HHwmWX2A="H - H was WARM / affectionate toward W - during discussion (2 rounds after)";
label HWwmHX2A="H - W was WARM / affectionate toward H - during discussion (2 rounds after)";
label WHwmWX2A="W - H was WARM / affectionate toward W - during discussion (2 rounds after)";
label WWwmHX2A="W - W was WARM / affectionate toward H - during discussion (2 rounds after)";

/**** Care - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcaWX2A,H9HcaWX2A,H10HcaWX2A);
%HWComb(HWcaHX2A,H9WcaHX2A,H10WcaHX2A);
%HWComb(WHcaWX2A,W9HcaWX2A,W10HcaWX2A);
%HWComb(WWcaHX2A,W9WcaHX2A,W10WcaHX2A);

label HHcaWX2A="H - H CARED about W opinions / feelings - during discussion (2 rounds after)";
label HWcaHX2A="H - W CARED about H opinions / feelings - during discussion (2 rounds after)";
label WHcaWX2A="W - H CARED about W opinions / feelings - during discussion (2 rounds after)";
label WWcaHX2A="W - W CARED about H opinions / feelings - during discussion (2 rounds after)";

/**** Understand - 'during VIDEO DISCUSSION' ****/
%HWComb(HHunWX2A,H9HunWX2A,H10HunWX2A);
%HWComb(HWunHX2A,H9WunHX2A,H10WunHX2A);
%HWComb(WHunWX2A,W9HunWX2A,W10HunWX2A);
%HWComb(WWunHX2A,W9WunHX2A,W10WunHX2A);

label HHunWX2A="H - H tried to UNDERSTAND W - during discussion (2 rounds after)";
label HWunHX2A="H - W tried to UNDERSTAND H - during discussion (2 rounds after)";
label WHunWX2A="W - H tried to UNDERSTAND W - during discussion (2 rounds after)";
label WWunHX2A="W - W tried to UNDERSTAND H - during discussion (2 rounds after)";

/**** Listened - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlsWX2A,H9HlsWX2A,H10HlsWX2A);
%HWComb(HWlsHX2A,H9WlsHX2A,H10WlsHX2A);
%HWComb(WHlsWX2A,W9HlsWX2A,W10HlsWX2A);
%HWComb(WWlsHX2A,W9WlsHX2A,W10WlsHX2A);

label HHlsWX2A="H - H LISTENED to W - during discussion (2 rounds after)";
label HWlsHX2A="H - W LISTENED to H - during discussion (2 rounds after)";
label WHlsWX2A="W - H LISTENED to W - during discussion (2 rounds after)";
label WWlsHX2A="W - W LISTENED to H - during discussion (2 rounds after)";

/**** Laugh - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlaWX2A,H9HlaWX2A,H10HlaWX2A);
%HWComb(HWlaHX2A,H9WlaHX2A,H10WlaHX2A);
%HWComb(WHlaWX2A,W9HlaWX2A,W10HlaWX2A);
%HWComb(WWlaHX2A,W9WlaHX2A,W10WlaHX2A);

label HHlaWX2A="H - H made W LAUGH and feel good - during discussion (2 rounds after)";
label HWlaHX2A="H - W made H LAUGH and feel good - during discussion (2 rounds after)";
label WHlaWX2A="W - H made W LAUGH and feel good - during discussion (2 rounds after)";
label WWlaHX2A="W - W made H LAUGH and feel good - during discussion (2 rounds after)";


/***************************************
	at 4A (4 rounds after)
****************************************/

/**** Warm - 'during VIDEO DISCUSSION' ****/
%HWComb(HHwmWX4A,H9HwmWX4A,H10HwmWX4A);
%HWComb(HWwmHX4A,H9WwmHX4A,H10WwmHX4A);
%HWComb(WHwmWX4A,W9HwmWX4A,W10HwmWX4A);
%HWComb(WWwmHX4A,W9WwmHX4A,W10WwmHX4A);

label HHwmWX4A="H - H was WARM / affectionate toward W - during discussion (4 rounds after)";
label HWwmHX4A="H - W was WARM / affectionate toward H - during discussion (4 rounds after)";
label WHwmWX4A="W - H was WARM / affectionate toward W - during discussion (4 rounds after)";
label WWwmHX4A="W - W was WARM / affectionate toward H - during discussion (4 rounds after)";

/**** Care - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcaWX4A,H9HcaWX4A,H10HcaWX4A);
%HWComb(HWcaHX4A,H9WcaHX4A,H10WcaHX4A);
%HWComb(WHcaWX4A,W9HcaWX4A,W10HcaWX4A);
%HWComb(WWcaHX4A,W9WcaHX4A,W10WcaHX4A);

label HHcaWX4A="H - H CARED about W opinions / feelings - during discussion (4 rounds after)";
label HWcaHX4A="H - W CARED about H opinions / feelings - during discussion (4 rounds after)";
label WHcaWX4A="W - H CARED about W opinions / feelings - during discussion (4 rounds after)";
label WWcaHX4A="W - W CARED about H opinions / feelings - during discussion (4 rounds after)";

/**** Understand - 'during VIDEO DISCUSSION' ****/
%HWComb(HHunWX4A,H9HunWX4A,H10HunWX4A);
%HWComb(HWunHX4A,H9WunHX4A,H10WunHX4A);
%HWComb(WHunWX4A,W9HunWX4A,W10HunWX4A);
%HWComb(WWunHX4A,W9WunHX4A,W10WunHX4A);

label HHunWX4A="H - H tried to UNDERSTAND W - during discussion (4 rounds after)";
label HWunHX4A="H - W tried to UNDERSTAND H - during discussion (4 rounds after)";
label WHunWX4A="W - H tried to UNDERSTAND W - during discussion (4 rounds after)";
label WWunHX4A="W - W tried to UNDERSTAND H - during discussion (4 rounds after)";

/**** Listened - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlsWX4A,H9HlsWX4A,H10HlsWX4A);
%HWComb(HWlsHX4A,H9WlsHX4A,H10WlsHX4A);
%HWComb(WHlsWX4A,W9HlsWX4A,W10HlsWX4A);
%HWComb(WWlsHX4A,W9WlsHX4A,W10WlsHX4A);

label HHlsWX4A="H - H LISTENED to W - during discussion (4 rounds after)";
label HWlsHX4A="H - W LISTENED to H - during discussion (4 rounds after)";
label WHlsWX4A="W - H LISTENED to W - during discussion (4 rounds after)";
label WWlsHX4A="W - W LISTENED to H - during discussion (4 rounds after)";

/**** Laugh - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlaWX4A,H9HlaWX4A,H10HlaWX4A);
%HWComb(HWlaHX4A,H9WlaHX4A,H10WlaHX4A);
%HWComb(WHlaWX4A,W9HlaWX4A,W10HlaWX4A);
%HWComb(WWlaHX4A,W9WlaHX4A,W10WlaHX4A);

label HHlaWX4A="H - H made W LAUGH and feel good - during discussion (4 rounds after)";
label HWlaHX4A="H - W made H LAUGH and feel good - during discussion (4 rounds after)";
label WHlaWX4A="W - H made W LAUGH and feel good - during discussion (4 rounds after)";
label WWlaHX4A="W - W made H LAUGH and feel good - during discussion (4 rounds after)";


/***************************************
	at 6A (6 rounds after)
****************************************/

/**** Warm - 'during VIDEO DISCUSSION' ****/
%HWComb(HHwmWX6A,H9HwmWX6A,H10HwmWX6A);
%HWComb(HWwmHX6A,H9WwmHX6A,H10WwmHX6A);
%HWComb(WHwmWX6A,W9HwmWX6A,W10HwmWX6A);
%HWComb(WWwmHX6A,W9WwmHX6A,W10WwmHX6A);

label HHwmWX6A="H - H was WARM / affectionate toward W - during discussion (6 rounds after)";
label HWwmHX6A="H - W was WARM / affectionate toward H - during discussion (6 rounds after)";
label WHwmWX6A="W - H was WARM / affectionate toward W - during discussion (6 rounds after)";
label WWwmHX6A="W - W was WARM / affectionate toward H - during discussion (6 rounds after)";

/**** Care - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcaWX6A,H9HcaWX6A,H10HcaWX6A);
%HWComb(HWcaHX6A,H9WcaHX6A,H10WcaHX6A);
%HWComb(WHcaWX6A,W9HcaWX6A,W10HcaWX6A);
%HWComb(WWcaHX6A,W9WcaHX6A,W10WcaHX6A);

label HHcaWX6A="H - H CARED about W opinions / feelings - during discussion (6 rounds after)";
label HWcaHX6A="H - W CARED about H opinions / feelings - during discussion (6 rounds after)";
label WHcaWX6A="W - H CARED about W opinions / feelings - during discussion (6 rounds after)";
label WWcaHX6A="W - W CARED about H opinions / feelings - during discussion (6 rounds after)";

/**** Understand - 'during VIDEO DISCUSSION' ****/
%HWComb(HHunWX6A,H9HunWX6A,H10HunWX6A);
%HWComb(HWunHX6A,H9WunHX6A,H10WunHX6A);
%HWComb(WHunWX6A,W9HunWX6A,W10HunWX6A);
%HWComb(WWunHX6A,W9WunHX6A,W10WunHX6A);

label HHunWX6A="H - H tried to UNDERSTAND W - during discussion (6 rounds after)";
label HWunHX6A="H - W tried to UNDERSTAND H - during discussion (6 rounds after)";
label WHunWX6A="W - H tried to UNDERSTAND W - during discussion (6 rounds after)";
label WWunHX6A="W - W tried to UNDERSTAND H - during discussion (6 rounds after)";

/**** Listened - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlsWX6A,H9HlsWX6A,H10HlsWX6A);
%HWComb(HWlsHX6A,H9WlsHX6A,H10WlsHX6A);
%HWComb(WHlsWX6A,W9HlsWX6A,W10HlsWX6A);
%HWComb(WWlsHX6A,W9WlsHX6A,W10WlsHX6A);

label HHlsWX6A="H - H LISTENED to W - during discussion (6 rounds after)";
label HWlsHX6A="H - W LISTENED to H - during discussion (6 rounds after)";
label WHlsWX6A="W - H LISTENED to W - during discussion (6 rounds after)";
label WWlsHX6A="W - W LISTENED to H - during discussion (6 rounds after)";

/**** Laugh - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlaWX6A,H9HlaWX6A,H10HlaWX6A);
%HWComb(HWlaHX6A,H9WlaHX6A,H10WlaHX6A);
%HWComb(WHlaWX6A,W9HlaWX6A,W10HlaWX6A);
%HWComb(WWlaHX6A,W9WlaHX6A,W10WlaHX6A);

label HHlaWX6A="H - H made W LAUGH and feel good - during discussion (6 rounds after)";
label HWlaHX6A="H - W made H LAUGH and feel good - during discussion (6 rounds after)";
label WHlaWX6A="W - H made W LAUGH and feel good - during discussion (6 rounds after)";
label WWlaHX6A="W - W made H LAUGH and feel good - during discussion (6 rounds after)";



/***************************************
	at 8A (8 rounds after)
****************************************/

/**** Warm - 'during VIDEO DISCUSSION' ****/
%HWComb(HHwmWX8A,H9HwmWX8A,H10HwmWX8A);
%HWComb(HWwmHX8A,H9WwmHX8A,H10WwmHX8A);
%HWComb(WHwmWX8A,W9HwmWX8A,W10HwmWX8A);
%HWComb(WWwmHX8A,W9WwmHX8A,W10WwmHX8A);

label HHwmWX8A="H - H was WARM / affectionate toward W - during discussion (8 rounds after)";
label HWwmHX8A="H - W was WARM / affectionate toward H - during discussion (8 rounds after)";
label WHwmWX8A="W - H was WARM / affectionate toward W - during discussion (8 rounds after)";
label WWwmHX8A="W - W was WARM / affectionate toward H - during discussion (8 rounds after)";

/**** Care - 'during VIDEO DISCUSSION' ****/
%HWComb(HHcaWX8A,H9HcaWX8A,H10HcaWX8A);
%HWComb(HWcaHX8A,H9WcaHX8A,H10WcaHX8A);
%HWComb(WHcaWX8A,W9HcaWX8A,W10HcaWX8A);
%HWComb(WWcaHX8A,W9WcaHX8A,W10WcaHX8A);

label HHcaWX8A="H - H CARED about W opinions / feelings - during discussion (8 rounds after)";
label HWcaHX8A="H - W CARED about H opinions / feelings - during discussion (8 rounds after)";
label WHcaWX8A="W - H CARED about W opinions / feelings - during discussion (8 rounds after)";
label WWcaHX8A="W - W CARED about H opinions / feelings - during discussion (8 rounds after)";

/**** Understand - 'during VIDEO DISCUSSION' ****/
%HWComb(HHunWX8A,H9HunWX8A,H10HunWX8A);
%HWComb(HWunHX8A,H9WunHX8A,H10WunHX8A);
%HWComb(WHunWX8A,W9HunWX8A,W10HunWX8A);
%HWComb(WWunHX8A,W9WunHX8A,W10WunHX8A);

label HHunWX8A="H - H tried to UNDERSTAND W - during discussion (8 rounds after)";
label HWunHX8A="H - W tried to UNDERSTAND H - during discussion (8 rounds after)";
label WHunWX8A="W - H tried to UNDERSTAND W - during discussion (8 rounds after)";
label WWunHX8A="W - W tried to UNDERSTAND H - during discussion (8 rounds after)";

/**** Listened - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlsWX8A,H9HlsWX8A,H10HlsWX8A);
%HWComb(HWlsHX8A,H9WlsHX8A,H10WlsHX8A);
%HWComb(WHlsWX8A,W9HlsWX8A,W10HlsWX8A);
%HWComb(WWlsHX8A,W9WlsHX8A,W10WlsHX8A);

label HHlsWX8A="H - H LISTENED to W - during discussion (8 rounds after)";
label HWlsHX8A="H - W LISTENED to H - during discussion (8 rounds after)";
label WHlsWX8A="W - H LISTENED to W - during discussion (8 rounds after)";
label WWlsHX8A="W - W LISTENED to H - during discussion (8 rounds after)";
/*   proc print data=rel5 (obs=25); 
var WWlsHX8A W9WlsHX8A W10WlsHX8A;
run;  */

/**** Laugh - 'during VIDEO DISCUSSION' ****/
%HWComb(HHlaWX8A,H9HlaWX8A,H10HlaWX8A);
%HWComb(HWlaHX8A,H9WlaHX8A,H10WlaHX8A);
%HWComb(WHlaWX8A,W9HlaWX8A,W10HlaWX8A);
%HWComb(WWlaHX8A,W9WlaHX8A,W10WlaHX8A);

label HHlaWX8A="H - H made W LAUGH and feel good - during discussion (8 rounds after)";
label HWlaHX8A="H - W made H LAUGH and feel good - during discussion (8 rounds after)";
label WHlaWX8A="W - H made W LAUGH and feel good - during discussion (8 rounds after)";
label WWlaHX8A="W - W made H LAUGH and feel good - during discussion (8 rounds after)";


/**************************************************************************************************************  
	Husbands' - wives' RELATIONSHIP INSTABILITY self-report (combines @@11 with @@12)  
	NOTICE that the following work only for Married/Cohabiting couples 
	--> variable ends with (0, 2A, 4A, 6A, 8A), not the one with trderscore (i.e., 0_, 2A_, 4A_, 6A_, 8A_ --> any partner)
***************************************************************************************************************/  

/*****************
	at TIME ZERO
******************/

/**** Seriously ****/
%HWComb(HHseW0,H11HseW0,H12HseW0);
%HWComb(HWseH0,H11WseH0,H12WseH0);
%HWComb(WHseW0,W11HseW0,W12HseW0);
%HWComb(WWseH0,W11WseH0,W12WseH0);

label HHseW2A="H - H seriously suggested ending rel/divorce  (time 0)";
label HWseH2A="H - W seriously suggested ending rel/divorce  (time 0)";
label WHseW2A="W - H seriously suggested ending rel/divorce  (time 0)";
label WWseH2A="W - W seriously suggested ending rel/divorce  (time 0)";

/**** Friend ****/
%HWComb(HHfrW0,H11HfrW0,H12HfrW0);
%HWComb(HWfrH0,H11WfrH0,H12WfrH0);
%HWComb(WHfrW0,W11HfrW0,W12HfrW0);
%HWComb(WWfrH0,W11WfrH0,W12WfrH0);

label HHfrW0="H - H talked to friend  (time 0)";
label HWfrH0="H - W talked to friend  (time 0)";
label WHfrW0="W - H talked to friend  (time 0)";
label WWfrH0="W - W talked to friend  (time 0)";

/**** Trouble ****/
%HWComb(HHtrW0,H11HtrW0,H12HtrW0);
%HWComb(HWtrH0,H11WtrH0,H12WtrH0);
%HWComb(WHtrW0,W11HtrW0,W12HtrW0);
%HWComb(WWtrH0,W11WtrH0,W12WtrH0);

label HHtrW0="H - H thought rel might be in trouble  (time 0)";
label HWtrH0="H - W thought rel might be in trouble  (time 0)";
label WHtrW0="W - H thought rel might be in trouble  (time 0)";
label WWtrH0="W - W thought rel might be in trouble  (time 0)";

/**** Others ****/
%HWComb(HHotW0,H11HotW0,H12HotW0);
%HWComb(HWotH0,H11WotH0,H12WotH0);
%HWComb(WHotW0,W11HotW0,W12HotW0);
%HWComb(WWotH0,W11WotH0,W12WotH0);

label HHotW0="H - H talked to attorney/others about leaving  (time 0)";
label HWotH0="H - W talked to attorney/others about leaving  (time 0)";
label WHotW0="W - H talked to attorney/others about leaving  (time 0)";
label WWotH0="W - W talked to attorney/others about leaving  (time 0)";

/**** Thought ****/
%HWComb(HHthW0,H11HthW0,H12HthW0);
%HWComb(HWthH0,H11WthH0,H12WthH0);
%HWComb(WHthW0,W11HthW0,W12HthW0);
%HWComb(WWthH0,W11WthH0,W12WthH0);

label HHthW0="H - H thought crossed mind  (time 0)";
label HWthH0="H - W thought crossed mind  (time 0)";
label WHthW0="W - H thought crossed mind  (time 0)";
label WWthH0="W - W thought crossed mind  (time 0)";


/***************************************
	at 2A (2 rounds after)
****************************************/
/**** Seriously ****/
%HWComb(HHseW2A,H11HseW2A,H12HseW2A);
%HWComb(HWseH2A,H11WseH2A,H12WseH2A);
%HWComb(WHseW2A,W11HseW2A,W12HseW2A);
%HWComb(WWseH2A,W11WseH2A,W12WseH2A);

label HHseW2A="H - H seriously suggested ending rel/divorce  (2 rounds after)";
label HWseH2A="H - W seriously suggested ending rel/divorce  (2 rounds after)";
label WHseW2A="W - H seriously suggested ending rel/divorce  (2 rounds after)";
label WWseH2A="W - W seriously suggested ending rel/divorce  (2 rounds after)";

/**** Friend ****/
%HWComb(HHfrW2A,H11HfrW2A,H12HfrW2A);
%HWComb(HWfrH2A,H11WfrH2A,H12WfrH2A);
%HWComb(WHfrW2A,W11HfrW2A,W12HfrW2A);
%HWComb(WWfrH2A,W11WfrH2A,W12WfrH2A);

label HHfrW2A="H - H talked to a close friend  (2 rounds after)";
label HWfrH2A="H - W talked to a close friend  (2 rounds after)";
label WHfrW2A="W - H talked to a close friend  (2 rounds after)";
label WWfrH2A="W - W talked to a close friend  (2 rounds after)";

/**** Trouble ****/
%HWComb(HHtrW2A,H11HtrW2A,H12HtrW2A);
%HWComb(HWtrH2A,H11WtrH2A,H12WtrH2A);
%HWComb(WHtrW2A,W11HtrW2A,W12HtrW2A);
%HWComb(WWtrH2A,W11WtrH2A,W12WtrH2A);

label HHtrW2A="H - H thought rel might be in trouble  (2 rounds after)";
label HWtrH2A="H - W thought rel might be in trouble  (2 rounds after)";
label WHtrW2A="W - H thought rel might be in trouble  (2 rounds after)";
label WWtrH2A="W - W thought rel might be in trouble  (2 rounds after)";

/**** Others ****/
%HWComb(HHotW2A,H11HotW2A,H12HotW2A);
%HWComb(HWotH2A,H11WotH2A,H12WotH2A);
%HWComb(WHotW2A,W11HotW2A,W12HotW2A);
%HWComb(WWotH2A,W11WotH2A,W12WotH2A);

label HHotW2A="H - H talked to attorney/others about leaving  (2 rounds after)";
label HWotH2A="H - W talked to attorney/others about leaving  (2 rounds after)";
label WHotW2A="W - H talked to attorney/others about leaving  (2 rounds after)";
label WWotH2A="W - W talked to attorney/others about leaving  (2 rounds after)";

/**** Thought ****/
%HWComb(HHthW2A,H11HthW2A,H12HthW2A);
%HWComb(HWthH2A,H11WthH2A,H12WthH2A);
%HWComb(WHthW2A,W11HthW2A,W12HthW2A);
%HWComb(WWthH2A,W11WthH2A,W12WthH2A);

label HHthW2A="H - H thought crossed mind  (2 rounds after)";
label HWthH2A="H - W thought crossed mind  (2 rounds after)";
label WHthW2A="W - H thought crossed mind  (2 rounds after)";
label WWthH2A="W - W thought crossed mind  (2 rounds after)";


/***************************************
	at 4A (4 rounds after)
****************************************/

/**** Seriously ****/
%HWComb(HHseW4A,H11HseW4A,H12HseW4A);
%HWComb(HWseH4A,H11WseH4A,H12WseH4A);
%HWComb(WHseW4A,W11HseW4A,W12HseW4A);
%HWComb(WWseH4A,W11WseH4A,W12WseH4A);

label HHseW4A="H - H seriously suggested ending rel/divorceW  (4 rounds after)";
label HWseH4A="H - W seriously suggested ending rel/divorceH  (4 rounds after)";
label WHseW4A="W - H seriously suggested ending rel/divorceW  (4 rounds after)";
label WWseH4A="W - W seriously suggested ending rel/divorceH  (4 rounds after)";

/**** Friend ****/
%HWComb(HHfrW4A,H11HfrW4A,H12HfrW4A);
%HWComb(HWfrH4A,H11WfrH4A,H12WfrH4A);
%HWComb(WHfrW4A,W11HfrW4A,W12HfrW4A);
%HWComb(WWfrH4A,W11WfrH4A,W12WfrH4A);

label HHfrW4A="H - H talked to a close friend  (4 rounds after)";
label HWfrH4A="H - W talked to a close friend  (4 rounds after)";
label WHfrW4A="W - H talked to a close friend  (4 rounds after)";
label WWfrH4A="W - W talked to a close friend  (4 rounds after)";

/**** Trouble ****/
%HWComb(HHtrW4A,H11HtrW4A,H12HtrW4A);
%HWComb(HWtrH4A,H11WtrH4A,H12WtrH4A);
%HWComb(WHtrW4A,W11HtrW4A,W12HtrW4A);
%HWComb(WWtrH4A,W11WtrH4A,W12WtrH4A);

label HHtrW4A="H - H thought rel might be in trouble  (4 rounds after)";
label HWtrH4A="H - W thought rel might be in trouble  (4 rounds after)";
label WHtrW4A="W - H thought rel might be in trouble  (4 rounds after)";
label WWtrH4A="W - W thought rel might be in trouble  (4 rounds after)";

/**** Others ****/
%HWComb(HHotW4A,H11HotW4A,H12HotW4A);
%HWComb(HWotH4A,H11WotH4A,H12WotH4A);
%HWComb(WHotW4A,W11HotW4A,W12HotW4A);
%HWComb(WWotH4A,W11WotH4A,W12WotH4A);

label HHotW4A="H - H talked to attorney/others about leaving  (4 rounds after)";
label HWotH4A="H - W talked to attorney/others about leaving  (4 rounds after)";
label WHotW4A="W - H talked to attorney/others about leaving  (4 rounds after)";
label WWotH4A="W - W talked to attorney/others about leaving  (4 rounds after)";

/**** Thought ****/
%HWComb(HHthW4A,H11HthW4A,H12HthW4A);
%HWComb(HWthH4A,H11WthH4A,H12WthH4A);
%HWComb(WHthW4A,W11HthW4A,W12HthW4A);
%HWComb(WWthH4A,W11WthH4A,W12WthH4A);

label HHthW4A="H - H thought crossed mind  (4 rounds after)";
label HWthH4A="H - W thought crossed mind  (4 rounds after)";
label WHthW4A="W - H thought crossed mind  (4 rounds after)";
label WWthH4A="W - W thought crossed mind  (4 rounds after)";


/***************************************
	at 6A (6 rounds after)
****************************************/

/**** Seriously ****/
%HWComb(HHseW6A,H11HseW6A,H12HseW6A);
%HWComb(HWseH6A,H11WseH6A,H12WseH6A);
%HWComb(WHseW6A,W11HseW6A,W12HseW6A);
%HWComb(WWseH6A,W11WseH6A,W12WseH6A);

label HHseW6A="H - H seriously suggested ending rel/divorce  (6 rounds after)";
label HWseH6A="H - W seriously suggested ending rel/divorce  (6 rounds after)";
label WHseW6A="W - H seriously suggested ending rel/divorce  (6 rounds after)";
label WWseH6A="W - W seriously suggested ending rel/divorce  (6 rounds after)";

/**** Friend ****/
%HWComb(HHfrW6A,H11HfrW6A,H12HfrW6A);
%HWComb(HWfrH6A,H11WfrH6A,H12WfrH6A);
%HWComb(WHfrW6A,W11HfrW6A,W12HfrW6A);
%HWComb(WWfrH6A,W11WfrH6A,W12WfrH6A);

label HHfrW6A="H - H talked to a close friend  (6 rounds after)";
label HWfrH6A="H - W talked to a close friend  (6 rounds after)";
label WHfrW6A="W - H talked to a close friend  (6 rounds after)";
label WWfrH6A="W - W talked to a close friend  (6 rounds after)";

/**** Trouble ****/
%HWComb(HHtrW6A,H11HtrW6A,H12HtrW6A);
%HWComb(HWtrH6A,H11WtrH6A,H12WtrH6A);
%HWComb(WHtrW6A,W11HtrW6A,W12HtrW6A);
%HWComb(WWtrH6A,W11WtrH6A,W12WtrH6A);

label HHtrW6A="H - H thought rel might be in trouble  (6 rounds after)";
label HWtrH6A="H - W thought rel might be in trouble  (6 rounds after)";
label WHtrW6A="W - H thought rel might be in trouble  (6 rounds after)";
label WWtrH6A="W - W thought rel might be in trouble  (6 rounds after)";

/**** Others ****/
%HWComb(HHotW6A,H11HotW6A,H12HotW6A);
%HWComb(HWotH6A,H11WotH6A,H12WotH6A);
%HWComb(WHotW6A,W11HotW6A,W12HotW6A);
%HWComb(WWotH6A,W11WotH6A,W12WotH6A);

label HHotW6A="H - H talked to attorney/others about leaving  (6 rounds after)";
label HWotH6A="H - W talked to attorney/others about leaving  (6 rounds after)";
label WHotW6A="W - H talked to attorney/others about leaving  (6 rounds after)";
label WWotH6A="W - W talked to attorney/others about leaving  (6 rounds after)";

/**** Thought ****/
%HWComb(HHthW6A,H11HthW6A,H12HthW6A);
%HWComb(HWthH6A,H11WthH6A,H12WthH6A);
%HWComb(WHthW6A,W11HthW6A,W12HthW6A);
%HWComb(WWthH6A,W11WthH6A,W12WthH6A);

label HHthW6A="H - H made W thought crossed mind  (6 rounds after)";
label HWthH6A="H - W made H thought crossed mind  (6 rounds after)";
label WHthW6A="W - H made W thought crossed mind  (6 rounds after)";
label WWthH6A="W - W made H thought crossed mind  (6 rounds after)";



/***************************************
	at 8A (8 rounds after)
****************************************/

/**** Seriously ****/
%HWComb(HHseW8A,H11HseW8A,H12HseW8A);
%HWComb(HWseH8A,H11WseH8A,H12WseH8A);
%HWComb(WHseW8A,W11HseW8A,W12HseW8A);
%HWComb(WWseH8A,W11WseH8A,W12WseH8A);

label HHseW8A="H - H seriously suggested ending rel/divorce  (8 rounds after)";
label HWseH8A="H - W seriously suggested ending rel/divorce  (8 rounds after)";
label WHseW8A="W - H seriously suggested ending rel/divorce  (8 rounds after)";
label WWseH8A="W - W seriously suggested ending rel/divorce  (8 rounds after)";

/**** Friend ****/
%HWComb(HHfrW8A,H11HfrW8A,H12HfrW8A);
%HWComb(HWfrH8A,H11WfrH8A,H12WfrH8A);
%HWComb(WHfrW8A,W11HfrW8A,W12HfrW8A);
%HWComb(WWfrH8A,W11WfrH8A,W12WfrH8A);

label HHfrW8A="H - H talked to a close friend  (8 rounds after)";
label HWfrH8A="H - W talked to a close friend  (8 rounds after)";
label WHfrW8A="W - H talked to a close friend  (8 rounds after)";
label WWfrH8A="W - W talked to a close friend  (8 rounds after)";

/**** Trouble ****/
%HWComb(HHtrW8A,H11HtrW8A,H12HtrW8A);
%HWComb(HWtrH8A,H11WtrH8A,H12WtrH8A);
%HWComb(WHtrW8A,W11HtrW8A,W12HtrW8A);
%HWComb(WWtrH8A,W11WtrH8A,W12WtrH8A);

label HHtrW8A="H - H thought rel might be in trouble  (8 rounds after)";
label HWtrH8A="H - W thought rel might be in trouble  (8 rounds after)";
label WHtrW8A="W - H thought rel might be in trouble  (8 rounds after)";
label WWtrH8A="W - W thought rel might be in trouble  (8 rounds after)";

/**** Others ****/
%HWComb(HHotW8A,H11HotW8A,H12HotW8A);
%HWComb(HWotH8A,H11WotH8A,H12WotH8A);
%HWComb(WHotW8A,W11HotW8A,W12HotW8A);
%HWComb(WWotH8A,W11WotH8A,W12WotH8A);

label HHotW8A="H - H talked to attorney/others about leaving  (8 rounds after)";
label HWotH8A="H - W talked to attorney/others about leaving  (8 rounds after)";
label WHotW8A="W - H talked to attorney/others about leaving  (8 rounds after)";
label WWotH8A="W - W talked to attorney/others about leaving  (8 rounds after)";

/**** Thought ****/
%HWComb(HHthW8A,H11HthW8A,H12HthW8A);
%HWComb(HWthH8A,H11WthH8A,H12WthH8A);
%HWComb(WHthW8A,W11HthW8A,W12HthW8A);
%HWComb(WWthH8A,W11WthH8A,W12WthH8A);

label HHthW8A="H - H made W thought crossed mind  (8 rounds after)";
label HWthH8A="H - W made H thought crossed mind  (8 rounds after)";
label WHthW8A="W - H made W thought crossed mind  (8 rounds after)";
label WWthH8A="W - W made H thought crossed mind  (8 rounds after)";

/*   proc print data=rel5 (obs=25); 
var WWotH8A W11WotH8A W12WotH8A;
rtr;  */


RUN;


/*** SAVING EVERYTHING AS PERMANENT SAS DATASET called "hsspri9507FULL"  ***/
data 'C:\Users\folorenz\Desktop\Time Shifting\hsspri9507FULL';
set rel5;
run;

