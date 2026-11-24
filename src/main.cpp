

#define SWITCH 1
#include <switch.h>
#include <sys/stat.h>
#include <SDL.h>
#include <SDL_ttf.h>
#include <curl/curl.h>
extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libswresample/swresample.h>
#include <libswscale/swscale.h>
#include <libavutil/opt.h>
#include <libavutil/time.h>
#include <libavutil/channel_layout.h>
}
#include <string>
#include <vector>
#include <deque>
#include <map>
#include <set>
#include <memory>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <atomic>
#include <functional>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cctype>
#include <cstdint>
#include <climits>
#include <algorithm>
#include <random>
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
static void logLine(const char* s);
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

struct Vec2 { float x=0,y=0; };
struct Vec3 { float x=0,y=0,z=0; };
static constexpr float DEG=(float)M_PI/180.f;
static float clampf(float v,float lo,float hi){ return v<lo?lo:(v>hi?hi:v); }
static SDL_Color C(int r,int g,int b,int a=255){ return SDL_Color{(Uint8)r,(Uint8)g,(Uint8)b,(Uint8)a}; }
static SDL_Color alpha(SDL_Color c,int a){ c.a=(Uint8)std::max(0,std::min(255,a)); return c; }
static SDL_Color mixc(SDL_Color a,SDL_Color b,float t){
    t=clampf(t,0,1);
    return C((int)(a.r+(b.r-a.r)*t),(int)(a.g+(b.g-a.g)*t),(int)(a.b+(b.b-a.b)*t),(int)(a.a+(b.a-a.a)*t));
}
static float lerpf(float a,float b,float t){ return a+(b-a)*t; }
static float easeOut(float t){ t=clampf(t,0,1); return 1-(1-t)*(1-t)*(1-t); }
static float wrapPi(float a){ while(a>(float)M_PI)a-=2*(float)M_PI; while(a<-(float)M_PI)a+=2*(float)M_PI; return a; }
static std::mt19937& rngRef(){
    static thread_local std::mt19937 g((uint32_t)(std::chrono::steady_clock::now().time_since_epoch().count()^(uintptr_t)&g));
    return g;
}
static float frand(float a,float b){ return std::uniform_real_distribution<float>(a,b)(rngRef()); }
static size_t randIndex(size_t n){ return n?(size_t)(rngRef()()%n):0; }
static tm tmUtc(time_t t){ tm r{}; gmtime_r(&t,&r); return r; }
static tm tmLocal(time_t t){ tm r{}; localtime_r(&t,&r); return r; }
static std::string lowerStr(std::string s){ for(auto& c:s)c=(char)tolower((unsigned char)c); return s; }
static std::string upperStr(std::string s){ for(auto& c:s)c=(char)toupper((unsigned char)c); return s; }
static std::string trimStr(const std::string& s){
    size_t a=0,b=s.size();
    while(a<b&&isspace((unsigned char)s[a]))++a;
    while(b>a&&isspace((unsigned char)s[b-1]))--b;
    return s.substr(a,b-a);
}
static void makeDir(const std::string& p){
    std::string q=p;
    while(q.size()>1&&(q.back()=='/'||q.back()=='\\'))q.pop_back();
    if(q.empty())return;
    mkdir(q.c_str(),0777);
}
static void makeDirs(const std::string& path){
    for(size_t i=1;i<path.size();i++)
        if(path[i]=='/'||path[i]=='\\'){ std::string s=path.substr(0,i); if(!s.empty()&&s.back()!=':')makeDir(s); }
    makeDir(path);
}
static void fft(float* re,float* im,int n){
    for(int i=1,j=0;i<n;i++){ int bit=n>>1; for(;j&bit;bit>>=1)j^=bit; j^=bit;
        if(i<j){ std::swap(re[i],re[j]); std::swap(im[i],im[j]); } }
    for(int len=2;len<=n;len<<=1){
        float ang=-2*(float)M_PI/len,wr=cosf(ang),wi=sinf(ang);
        for(int i=0;i<n;i+=len){ float cr=1,ci=0;
            for(int k=0;k<len/2;k++){
                float ur=re[i+k],ui=im[i+k],xr=re[i+k+len/2],xi=im[i+k+len/2];
                float vr=xr*cr-xi*ci,vi=xr*ci+xi*cr;
                re[i+k]=ur+vr; im[i+k]=ui+vi; re[i+k+len/2]=ur-vr; im[i+k+len/2]=ui-vi;
                float nc=cr*wr-ci*wi; ci=cr*wi+ci*wr; cr=nc; } } }
}
static Vec3 ll2v(float lon,float lat){
    float la=lat*DEG,lo=lon*DEG,cl=cosf(la);
    return {cl*sinf(lo),sinf(la),cl*cosf(lo)};
}
static std::atomic<bool> gQuit{false};

namespace Pal {
const SDL_Color chip=C(0,0,0), card=C(255,255,255,6), cardHi=C(255,255,255,14);
const SDL_Color text=C(245,246,248), dim=C(150,153,160), faint=C(96,99,106);
const SDL_Color accent=C(46,128,255), accentSoft=C(120,180,255), dot=C(46,128,255);
const SDL_Color live=C(60,220,130), liveSoft=C(130,240,180);
const SDL_Color warm=C(240,186,110), warmSoft=C(255,214,160), danger=C(235,110,110);
const SDL_Color wire=C(255,255,255);
const SDL_Color sphereIn=C(16,19,27), sphereOut=C(5,6,10);
const SDL_Color nightTint=C(26,30,66), dawnTint=C(18,54,62), dayTint=C(26,29,34), duskTint=C(58,38,20);
}
#define RNX_NAME    "RadioNX"
#define RNX_VERSION "1.0.0"
#define RNX_AUTHOR  "Muveeer"

static const char* UA_BROWSER="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36";
static const char* UA_APP=RNX_NAME "/" RNX_VERSION " (Nintendo Switch homebrew radio)";
static const char* UA_WIKI=RNX_NAME "/" RNX_VERSION " (Nintendo Switch homebrew radio; postcard feature; https://github.com/Muveeer/RadioNX)";

static const int BTN_SELECT=SDL_CONTROLLER_BUTTON_B, BTN_BACK=SDL_CONTROLLER_BUTTON_A;
static const int BTN_FAV   =SDL_CONTROLLER_BUTTON_Y, BTN_RANDOM=SDL_CONTROLLER_BUTTON_X;

struct J {
    const char* p; const char* e; bool ok=true;
    explicit J(const std::string& s):p(s.data()),e(s.data()+s.size()){}
    void ws(){ while(p<e&&(unsigned char)*p<=' ')++p; }
    bool at(char c){ ws(); return ok&&p<e&&*p==c; }
    bool eat(char c){ if(at(c)){++p;return true;} ok=false; return false; }
    std::string str(){
        ws(); std::string o;
        if(p>=e||*p!='"'){ok=false;return o;}
        ++p;
        while(p<e&&*p!='"'){
            char c=*p++;
            if(c=='\\'&&p<e){
                char k=*p++;
                switch(k){
                    case 'n':o+='\n';break; case 't':o+='\t';break; case 'r':o+='\r';break;
                    case 'b':o+='\b';break; case 'f':o+='\f';break;
                    case 'u':{
                        unsigned cp=0;
                        for(int i=0;i<4&&p<e;i++){ char h=*p++; cp<<=4;
                            cp+=(h>='0'&&h<='9')?h-'0':((h|32)>='a'&&(h|32)<='f')?(h|32)-'a'+10:0; }
                        if(cp>=0xD800&&cp<=0xDBFF&&e-p>=6&&p[0]=='\\'&&p[1]=='u'){
                            p+=2; unsigned lo=0;
                            for(int i=0;i<4&&p<e;i++){ char h=*p++; lo<<=4;
                                lo+=(h>='0'&&h<='9')?h-'0':((h|32)>='a'&&(h|32)<='f')?(h|32)-'a'+10:0; }
                            cp=0x10000+((cp-0xD800)<<10)+(lo-0xDC00);
                        }
                        if(cp<0x80)o+=(char)cp;
                        else if(cp<0x800){o+=(char)(0xC0|(cp>>6));o+=(char)(0x80|(cp&63));}
                        else if(cp<0x10000){o+=(char)(0xE0|(cp>>12));o+=(char)(0x80|((cp>>6)&63));o+=(char)(0x80|(cp&63));}
                        else{o+=(char)(0xF0|(cp>>18));o+=(char)(0x80|((cp>>12)&63));o+=(char)(0x80|((cp>>6)&63));o+=(char)(0x80|(cp&63));}
                        break;}
                    default:o+=k;
                }
            } else o+=c;
        }
        if(p<e)++p; else ok=false;
        return o;
    }
    double num(){ ws(); char* q=nullptr; double d=strtod(p,&q); if(!q||q==p||q>e){ok=false;return 0;} p=q; return d; }
    bool boolv(){ ws();
        if(e-p>=4&&p[0]=='t'){p+=4;return true;}
        if(e-p>=5&&p[0]=='f'){p+=5;return false;}
        ok=false; return false; }
    void skip(){
        ws(); if(p>=e){ok=false;return;}
        char c=*p;
        if(c=='"'){ str(); return; }
        if(c=='{'||c=='['){
            int d=0;
            while(p<e){
                char ch=*p;
                if(ch=='"'){ str(); if(!ok)return; continue; }
                if(ch=='{'||ch=='[')++d;
                else if(ch=='}'||ch==']'){ --d; ++p; if(d==0)return; continue; }
                ++p;
            }
            ok=false; return;
        }
        if(e-p>=4&&p[0]=='t'){p+=4;return;}
        if(e-p>=5&&p[0]=='f'){p+=5;return;}
        if(e-p>=4&&p[0]=='n'){p+=4;return;}
        num();
    }
};
static bool objNext(J& j,std::string& key){
    j.ws();
    if(j.ok&&j.p<j.e&&*j.p==','){++j.p;j.ws();}
    if(!j.ok||j.p>=j.e||*j.p=='}')return false;
    key=j.str();
    if(!j.ok)return false;
    return j.eat(':');
}
static bool arrNext(J& j){
    j.ws();
    if(j.ok&&j.p<j.e&&*j.p==','){++j.p;j.ws();}
    return j.ok&&j.p<j.e&&*j.p!=']';
}
static std::string jsonEsc(const std::string& s){
    std::string o;
    for(unsigned char c:s){
        if(c=='"'||c=='\\'){o+='\\';o+=(char)c;}
        else if(c=='\n')o+="\\n"; else if(c=='\r')o+="\\r"; else if(c=='\t')o+="\\t";
        else if(c<0x20){char b[8];snprintf(b,8,"\\u%04x",c);o+=b;}
        else o+=(char)c;
    }
    return o;
}
static std::string urlEsc(const std::string& s){
    CURL* c=curl_easy_init(); if(!c)return s;
    char* e=curl_easy_escape(c,s.c_str(),0);
    std::string r=e?e:""; if(e)curl_free(e);
    curl_easy_cleanup(c); return r;
}
static std::string lastSeg(const std::string& url){
    size_t k=url.find_last_of('/');
    if(k==std::string::npos)return "";
    std::string s=url.substr(k+1);
    size_t q=s.find_first_of("?#");
    return q==std::string::npos?s:s.substr(0,q);
}

struct Batch {
    std::vector<SDL_Vertex> v; std::vector<int> idx;
    Batch(){ v.reserve(8192); idx.reserve(16384); }
    void clear(){ v.clear(); idx.clear(); }
    int vert(float x,float y,SDL_Color c){ v.push_back(SDL_Vertex{{x,y},c,{0,0}}); return (int)v.size()-1; }
    void tri(int a,int b,int c){ idx.push_back(a);idx.push_back(b);idx.push_back(c); }
    void quad(float x0,float y0,float x1,float y1,float x2,float y2,float x3,float y3,SDL_Color c){
        int a=vert(x0,y0,c),b=vert(x1,y1,c),d=vert(x2,y2,c),e=vert(x3,y3,c); tri(a,b,d); tri(a,d,e);
    }
    void gradRect(float x,float y,float w,float h,SDL_Color t,SDL_Color b){
        int p=vert(x,y,t),q=vert(x+w,y,t),r=vert(x+w,y+h,b),s=vert(x,y+h,b); tri(p,q,r); tri(p,r,s);
    }
    void circle(float cx,float cy,float r,SDL_Color c,int seg=12){
        if(r<=0)return; int base=vert(cx,cy,c);
        for(int i=0;i<seg;i++){float a=2*(float)M_PI*i/seg; vert(cx+cosf(a)*r,cy+sinf(a)*r,c);}
        for(int i=0;i<seg;i++)tri(base,base+1+i,base+1+(i+1)%seg);
    }
    void gradCircle(float cx,float cy,float r,SDL_Color in,SDL_Color out,int seg=36){
        if(r<=0)return; int base=vert(cx,cy,in);
        for(int i=0;i<seg;i++){float a=2*(float)M_PI*i/seg; vert(cx+cosf(a)*r,cy+sinf(a)*r,out);}
        for(int i=0;i<seg;i++)tri(base,base+1+i,base+1+(i+1)%seg);
    }
    void ring(float cx,float cy,float r,float th,SDL_Color c,int seg=36,float a0=0,float a1=2*(float)M_PI){
        if(r<=0)return;
        for(int i=0;i<seg;i++){
            float t0=a0+(a1-a0)*i/seg,t1=a0+(a1-a0)*(i+1)/seg;
            float c0=cosf(t0),s0=sinf(t0),c1=cosf(t1),s1=sinf(t1);
            quad(cx+c0*(r-th/2),cy+s0*(r-th/2),cx+c0*(r+th/2),cy+s0*(r+th/2),
                 cx+c1*(r+th/2),cy+s1*(r+th/2),cx+c1*(r-th/2),cy+s1*(r-th/2),c);
        }
    }
    void line(float x0,float y0,float x1,float y1,float th,SDL_Color c){
        float dx=x1-x0,dy=y1-y0,l=sqrtf(dx*dx+dy*dy); if(l<1e-4f)return;
        float nx=-dy/l*th/2,ny=dx/l*th/2;
        quad(x0+nx,y0+ny,x0-nx,y0-ny,x1-nx,y1-ny,x1+nx,y1+ny,c);
    }
    void rrect2(float x,float y,float w,float h,float rt,float rb,SDL_Color c){
        if(w<=0||h<=0)return;
        rt=std::min(rt,std::min(w,h)/2); rb=std::min(rb,std::min(w,h)/2);
        int base=vert(x+w/2,y+h/2,c);
        const float cx[4]={x+w-rt,x+w-rb,x+rb,x+rt}, cy[4]={y+rt,y+h-rb,y+h-rb,y+rt};
        const float rr[4]={rt,rb,rb,rt}, a0[4]={-(float)M_PI/2,0,(float)M_PI/2,(float)M_PI};
        int first=-1,prev=-1;
        for(int k=0;k<4;k++)for(int i=0;i<=4;i++){
            float a=a0[k]+(float)M_PI/2*i/4;
            int id=vert(cx[k]+cosf(a)*rr[k],cy[k]+sinf(a)*rr[k],c);
            if(first<0)first=id;
            if(prev>=0)tri(base,prev,id);
            prev=id;
        }
        tri(base,prev,first);
    }
    void rrect(float x,float y,float w,float h,float r,SDL_Color c){ rrect2(x,y,w,h,r,r,c); }
    void glowRing(float cx,float cy,float r0,float r1,SDL_Color cin,SDL_Color cout,int seg=64){
        if(r1<=r0)return;
        for(int i=0;i<seg;i++){
            float a0=2*(float)M_PI*i/seg,a1=2*(float)M_PI*(i+1)/seg;
            float c0=cosf(a0),s0=sinf(a0),c1=cosf(a1),s1=sinf(a1);
            int p0=vert(cx+c0*r0,cy+s0*r0,cin);
            int p1=vert(cx+c1*r0,cy+s1*r0,cin);
            int p2=vert(cx+c1*r1,cy+s1*r1,cout);
            int p3=vert(cx+c0*r1,cy+s0*r1,cout);
            tri(p0,p1,p2); tri(p0,p2,p3);
        }
    }
    void flush(SDL_Renderer* r){
        if(!idx.empty())SDL_RenderGeometry(r,nullptr,v.data(),(int)v.size(),idx.data(),(int)idx.size());
        clear();
    }
};

namespace Icon {
void play(Batch& b,float x,float y,float s,SDL_Color c){
    b.tri(b.vert(x-s*.38f,y-s*.55f,c),b.vert(x-s*.38f,y+s*.55f,c),b.vert(x+s*.62f,y,c)); }
void pause(Batch& b,float x,float y,float s,SDL_Color c){
    b.quad(x-s*.42f,y-s*.55f,x-s*.14f,y-s*.55f,x-s*.14f,y+s*.55f,x-s*.42f,y+s*.55f,c);
    b.quad(x+s*.14f,y-s*.55f,x+s*.42f,y-s*.55f,x+s*.42f,y+s*.55f,x+s*.14f,y+s*.55f,c); }
void heart(Batch& b,float x,float y,float s,SDL_Color c,bool fill){
    if(fill){ b.circle(x-s*.25f,y-s*.18f,s*.3f,c,8); b.circle(x+s*.25f,y-s*.18f,s*.3f,c,8);
        b.tri(b.vert(x-s*.53f,y-s*.05f,c),b.vert(x+s*.53f,y-s*.05f,c),b.vert(x,y+s*.55f,c)); }
    else { b.line(x-s*.55f,y-s*.18f,x,y+s*.55f,2,c); b.line(x+s*.55f,y-s*.18f,x,y+s*.55f,2,c);
        b.ring(x-s*.25f,y-s*.18f,s*.3f,2,c,8,(float)M_PI,2*(float)M_PI);
        b.ring(x+s*.25f,y-s*.18f,s*.3f,2,c,8,(float)M_PI,2*(float)M_PI); } }
void search(Batch& b,float x,float y,float s,SDL_Color c){
    b.ring(x-s*.12f,y-s*.12f,s*.42f,2.2f,c,14); b.line(x+s*.2f,y+s*.2f,x+s*.55f,y+s*.55f,2.4f,c); }
void back(Batch& b,float x,float y,float s,SDL_Color c){
    b.line(x+s*.3f,y-s*.45f,x-s*.25f,y,2.4f,c); b.line(x-s*.25f,y,x+s*.3f,y+s*.45f,2.4f,c); }
void globe(Batch& b,float x,float y,float s,SDL_Color c){
    b.ring(x,y,s*.62f,1.7f,c,18); b.line(x-s*.62f,y,x+s*.62f,y,1.3f,alpha(c,150));
    b.ring(x,y,s*.3f,1.3f,alpha(c,150),12); }
void spinner(Batch& b,float x,float y,float r,float t,SDL_Color c){ b.ring(x,y,r,3.f,c,20,t,t+4.4f); }
void clock(Batch& b,float x,float y,float r,SDL_Color c){
    b.ring(x,y,r,1.6f,c,16); b.line(x,y,x,y-r*.55f,1.6f,c); b.line(x,y,x+r*.45f,y+r*.15f,1.6f,c); }
void sun(Batch& b,float x,float y,float r,SDL_Color c){
    b.circle(x,y,r*.5f,c,10);
    for(int i=0;i<8;i++){ float a=i*(float)M_PI/4;
        b.line(x+cosf(a)*r*.78f,y+sinf(a)*r*.78f,x+cosf(a)*r*1.12f,y+sinf(a)*r*1.12f,1.5f,c); } }
void cloud(Batch& b,float x,float y,float r,SDL_Color c){
    b.circle(x-r*.38f,y+r*.05f,r*.42f,c,10); b.circle(x+r*.22f,y-r*.12f,r*.5f,c,10);
    b.quad(x-r*.8f,y+r*.05f,x+r*.72f,y+r*.05f,x+r*.72f,y+r*.47f,x-r*.8f,y+r*.47f,c); }
void weather(Batch& b,float x,float y,float r,int code,SDL_Color c){
    if(code==0){ sun(b,x,y,r,c); return; }
    if(code<=2){ sun(b,x+r*.42f,y-r*.42f,r*.62f,alpha(c,190)); cloud(b,x-r*.1f,y+r*.12f,r*.86f,c); return; }
    if(code==45||code==48){ for(int i=0;i<3;i++)b.line(x-r,y-r*.4f+i*r*.5f,x+r,y-r*.4f+i*r*.5f,1.8f,alpha(c,200-i*40)); return; }
    cloud(b,x,y-r*.18f,r*.9f,c);
    if((code>=71&&code<=77)||code==85||code==86){ for(int i=0;i<3;i++)b.circle(x-r*.5f+i*r*.5f,y+r*.62f,1.7f,c,6); return; }
    if(code>=95){ b.tri(b.vert(x+r*.1f,y+r*.25f,c),b.vert(x-r*.28f,y+r*.95f,c),b.vert(x+r*.02f,y+r*.62f,c));
        b.tri(b.vert(x+r*.02f,y+r*.62f,c),b.vert(x+r*.34f,y+r*.3f,c),b.vert(x-r*.02f,y+r*1.05f,c)); return; }
    for(int i=0;i<3;i++)b.line(x-r*.5f+i*r*.5f,y+r*.42f,x-r*.62f+i*r*.5f,y+r*.9f,1.7f,c);
}
}

struct Sky {
    struct Star { float x,y,r,ph,sp; };
    struct Mote { float x,y,vx,vy,a; };
    std::vector<Star> stars; Mote motes[4];
    bool shOn=false; float shX=0,shY=0,shA=0,shSp=0,shLen=0,shT=0,shDur=1,nextShoot=8;
    void init(){
        stars.clear();
        for(int i=0;i<20;i++){ Star s; s.x=frand(0,1280); s.y=frand(0,720);
            s.r=frand(.7f,1.5f); s.ph=frand(0,6.28f); s.sp=frand(.2f,.7f); stars.push_back(s); }
        for(int i=0;i<4;i++)respawn(motes[i],true);
    }
    void respawn(Mote& m,bool any){ m.x=frand(0,1280); m.y=any?frand(0,720):720.f+frand(0,60);
        m.vx=frand(-6,6); m.vy=frand(-9,-3); m.a=frand(10,22); }
    void update(float dt){
        nextShoot-=dt;
        if(!shOn&&nextShoot<=0){ shOn=true; shT=0; shX=frand(60,1220); shY=frand(30,520);
            shA=frand(0,2*(float)M_PI); shSp=frand(240,520); shLen=frand(70,160);
            shDur=frand(.9f,1.5f); nextShoot=frand(14,32); }
        if(shOn){ shT+=dt; if(shT>=shDur)shOn=false; }
        for(int i=0;i<4;i++){ Mote& m=motes[i]; m.x+=m.vx*dt; m.y+=m.vy*dt;
            if(m.y<-20||m.x<-20||m.x>1300)respawn(m,false); }
    }
    void draw(Batch& b,float t,float par,float nb){
        for(auto& s:stars){ float k=.5f+.5f*sinf(t*s.sp+s.ph);
            b.circle(s.x+par*3,s.y,s.r,alpha(Pal::text,(int)((16+46*k)*(1.f+.5f*nb))),6); }
        for(int i=0;i<4;i++){ Mote& m=motes[i]; b.circle(m.x,m.y,1.1f,alpha(Pal::warmSoft,(int)m.a),6); }
        if(shOn){
            float p=shT/shDur,a=sinf((float)M_PI*p);
            float hx=shX+cosf(shA)*shSp*shT, hy=shY+sinf(shA)*shSp*shT;
            for(int k=0;k<4;k++){ float f0=k/4.f,f1=(k+1)/4.f;
                b.line(hx-cosf(shA)*shLen*f1,hy-sinf(shA)*shLen*f1,
                       hx-cosf(shA)*shLen*f0,hy-sinf(shA)*shLen*f0,1.6f*(1.f-f0*.6f),alpha(Pal::text,(int)(110*a*(1.f-f0)))); }
            b.circle(hx,hy,1.7f,alpha(Pal::text,(int)(150*a)),6);
        }
    }
};

static std::string gAppDir="./", gCacheDir="./";
static bool fileExists(const std::string& p){ FILE* f=fopen(p.c_str(),"rb"); if(f){fclose(f);return true;} return false; }
static bool readFile(const std::string& p,std::string& o){
    FILE* f=fopen(p.c_str(),"rb"); if(!f)return false;
    char b[65536]; size_t n; o.clear();
    while((n=fread(b,1,sizeof b,f))>0)o.append(b,n);
    fclose(f); return true;
}
static void writeFile(const std::string& p,const std::string& d){
    FILE* f=fopen(p.c_str(),"wb"); if(!f)return; fwrite(d.data(),1,d.size(),f); fclose(f);
}
static std::string dirOf(const std::string& p){
    size_t k=p.find_last_of("/\\"); return k==std::string::npos?"./":p.substr(0,k+1);
}
static void initPaths(const char* argv0){
    gAppDir=argv0?dirOf(argv0):"sdmc:/switch/RadioNX/";
    if(gAppDir.rfind("sdmc:",0)!=0)gAppDir="sdmc:/switch/RadioNX/";
    gCacheDir="sdmc:/switch/RadioNX/data/";
    makeDirs("sdmc:/switch/RadioNX/data");
}
static std::string asset(const std::string& n){
    for(const std::string& p:{gAppDir+"assets/"+n,std::string("./assets/")+n,gAppDir+n,
                              std::string("romfs:/")+n,std::string("romfs:/assets/")+n})
        if(fileExists(p))return p;
    return "";
}
static int curlAbort(void*,curl_off_t,curl_off_t,curl_off_t,curl_off_t){ return gQuit.load()?1:0; }
static size_t curlWrite(char* p,size_t s,size_t n,void* ud){
    std::string* o=(std::string*)ud; size_t t=s*n;
    if(o->size()+t>16u*1024u*1024u)return 0;
    o->append(p,t); return t;
}
static std::string httpGet(const std::string& url,std::string& err,long timeout=20,bool browser=true,bool json=true,const char* ua=nullptr){
    std::string out; err.clear();
    if(gQuit.load())return out;
    CURL* c=curl_easy_init(); if(!c){err="curl init";return out;}
    curl_slist* h=nullptr;
    h=curl_slist_append(h,"Accept: application/json, text/plain, */*");
    h=curl_slist_append(h,"Accept-Language: en-US,en;q=0.9");
    h=curl_slist_append(h,"Referer: https://radio.garden/");
    curl_easy_setopt(c,CURLOPT_URL,url.c_str());
    curl_easy_setopt(c,CURLOPT_HTTPHEADER,h);
    curl_easy_setopt(c,CURLOPT_WRITEFUNCTION,curlWrite);
    curl_easy_setopt(c,CURLOPT_WRITEDATA,&out);
    curl_easy_setopt(c,CURLOPT_FOLLOWLOCATION,1L);
    curl_easy_setopt(c,CURLOPT_MAXREDIRS,5L);
    curl_easy_setopt(c,CURLOPT_ACCEPT_ENCODING,"");
    curl_easy_setopt(c,CURLOPT_USERAGENT,ua?ua:(browser?UA_BROWSER:UA_APP));
    curl_easy_setopt(c,CURLOPT_TIMEOUT,timeout);
    curl_easy_setopt(c,CURLOPT_CONNECTTIMEOUT,8L);
    curl_easy_setopt(c,CURLOPT_NOSIGNAL,1L);
    curl_easy_setopt(c,CURLOPT_NOPROGRESS,0L);
    curl_easy_setopt(c,CURLOPT_XFERINFOFUNCTION,curlAbort);
    curl_easy_setopt(c,CURLOPT_TCP_KEEPALIVE,1L);
    std::string ca=asset("cacert.pem");
    if(!ca.empty())curl_easy_setopt(c,CURLOPT_CAINFO,ca.c_str());
    CURLcode r=curl_easy_perform(c);
    long code=0; curl_easy_getinfo(c,CURLINFO_RESPONSE_CODE,&code);
    curl_slist_free_all(h); curl_easy_cleanup(c);
    if(gQuit.load())return "";
    if(r!=CURLE_OK){err=curl_easy_strerror(r);return "";}
    if(code>=400){err="HTTP "+std::to_string(code);return "";}
    if(out.empty()){err="empty response";return "";}
    if(json&&out[0]!='{'&&out[0]!='['){err="bad response ("+std::to_string(code)+")";return "";}
    return out;
}

static const char* RG="https://radio.garden/api";
static std::string rgStream(const std::string& id){ return std::string(RG)+"/ara/content/listen/"+id+"/channel.mp3"; }
struct Place { std::string id,title,country; float lon=0,lat=0; int size=0; };
struct Channel { std::string id,title,url,placeId,place,country,cc; float lat=0,lon=0; };
struct CityRes { std::string placeId,title,err; int count=0,utcOffset=0;
    bool haveOffset=false,ok=false; std::vector<Channel> ch; };
struct SearchHit { bool isPlace=false; std::string title,sub,id,cc,country,place,placeId;
    int count=0; float lat=0,lon=0; };
struct PlacesRes { std::vector<Place> v; std::string version,err; bool cache=false; };
struct RandomRes { Channel ch; std::string err; };
struct WxRes { bool ok=false; float temp=0; int code=0,tz=0; std::string key; };
struct FlagRes { std::string iso; int w=0,h=0; std::vector<uint8_t> rgba; bool ok=false; };
struct GeoRes { bool ok=false; float lat=0,lon=0; };

static const char* ISO_TBL=
"united states=us;usa=us;united kingdom=gb;uk=gb;russia=ru;japan=jp;india=in;brazil=br;germany=de;france=fr;"
"italy=it;spain=es;canada=ca;australia=au;china=cn;mexico=mx;argentina=ar;netherlands=nl;poland=pl;ukraine=ua;"
"turkey=tr;türkiye=tr;south korea=kr;korea=kr;indonesia=id;thailand=th;vietnam=vn;philippines=ph;egypt=eg;"
"south africa=za;nigeria=ng;kenya=ke;saudi arabia=sa;sweden=se;norway=no;finland=fi;denmark=dk;ireland=ie;"
"portugal=pt;greece=gr;czech republic=cz;czechia=cz;austria=at;switzerland=ch;belgium=be;romania=ro;hungary=hu;"
"new zealand=nz;chile=cl;colombia=co;peru=pe;venezuela=ve;pakistan=pk;bangladesh=bd;iran=ir;iraq=iq;israel=il;"
"morocco=ma;algeria=dz;malaysia=my;singapore=sg;taiwan=tw;hong kong=hk;croatia=hr;serbia=rs;bulgaria=bg;"
"slovakia=sk;slovenia=si;lithuania=lt;latvia=lv;estonia=ee;iceland=is;cuba=cu;ecuador=ec;bolivia=bo;paraguay=py;"
"uruguay=uy;costa rica=cr;panama=pa;guatemala=gt;dominican republic=do;jamaica=jm;ghana=gh;ethiopia=et;"
"tanzania=tz;uganda=ug;zimbabwe=zw;zambia=zm;mozambique=mz;angola=ao;cameroon=cm;senegal=sn;tunisia=tn;libya=ly;"
"sudan=sd;yemen=ye;oman=om;qatar=qa;kuwait=kw;united arab emirates=ae;jordan=jo;lebanon=lb;syria=sy;"
"afghanistan=af;nepal=np;sri lanka=lk;myanmar=mm;cambodia=kh;mongolia=mn;kazakhstan=kz;uzbekistan=uz;belarus=by;"
"moldova=md;georgia=ge;armenia=am;azerbaijan=az;albania=al;macedonia=mk;north macedonia=mk;"
"bosnia and herzegovina=ba;montenegro=me;malta=mt;cyprus=cy;fiji=fj;papua new guinea=pg;samoa=ws;tonga=to;"
"palau=pw;brunei=bn;maldives=mv;bhutan=bt;haiti=ht;honduras=hn;nicaragua=ni;el salvador=sv;belize=bz;guyana=gy;"
"suriname=sr;trinidad and tobago=tt;barbados=bb;bahamas=bs;puerto rico=pr;guam=gu;martinique=mq;guadeloupe=gp;"
"reunion=re;mauritius=mu;seychelles=sc;cape verde=cv;madagascar=mg;gabon=ga;chad=td;niger=ne;mali=ml;"
"burkina faso=bf;benin=bj;togo=tg;sierra leone=sl;liberia=lr;guinea=gn;gambia=gm;mauritania=mr;south sudan=ss;"
"congo=cg;ivory coast=ci;cote d'ivoire=ci;kosovo=xk;palestine=ps;andorra=ad;monaco=mc;san marino=sm;"
"liechtenstein=li;greenland=gl;faroe islands=fo;jersey=je;gibraltar=gi;luxembourg=lu";
static std::string isoForCountry(const std::string& name){
    static std::map<std::string,std::string> m;
    if(m.empty()){
        std::string s=ISO_TBL; size_t i=0;
        while(i<s.size()){
            size_t j=s.find(';',i); if(j==std::string::npos)j=s.size();
            std::string kv=s.substr(i,j-i); size_t e=kv.find('=');
            if(e!=std::string::npos)m[kv.substr(0,e)]=kv.substr(e+1);
            i=j+1;
        }
    }
    auto it=m.find(lowerStr(trimStr(name)));
    return it==m.end()?"":it->second;
}
static PlacesRes parsePlaces(const std::string& body){
    PlacesRes r; J j(body);
    if(!j.eat('{'))return r;
    std::string k;
    while(objNext(j,k)){
        if(k=="data"){
            if(!j.eat('{'))break;
            std::string k2;
            while(objNext(j,k2)){
                if(k2=="version")r.version=j.str();
                else if(k2=="list"){
                    if(!j.eat('['))break;
                    while(arrNext(j)){
                        Place p;
                        if(!j.eat('{'))break;
                        std::string pk;
                        while(objNext(j,pk)){
                            if(pk=="id")p.id=j.str();
                            else if(pk=="title")p.title=j.str();
                            else if(pk=="country")p.country=j.str();
                            else if(pk=="size")p.size=(int)j.num();
                            else if(pk=="geo"){
                                if(j.eat('[')){ p.lon=(float)j.num(); if(arrNext(j))p.lat=(float)j.num();
                                    while(arrNext(j))j.skip(); j.eat(']'); }
                            }
                            else j.skip();
                        }
                        j.eat('}');
                        if(!j.ok)break;
                        if(!p.id.empty()&&p.size>0)r.v.push_back(std::move(p));
                    }
                    j.eat(']');
                }
                else j.skip();
            }
            j.eat('}');
        } else j.skip();
    }
    return r;
}
static PlacesRes fetchPlaces(){
    PlacesRes r;
    std::string cp=gCacheDir+"places.json",cached;
    std::string body=httpGet(std::string(RG)+"/ara/content/places",r.err,35);
    bool fromCache=false;
    if(!body.empty())writeFile(cp,body);
    else if(readFile(cp,cached)){ body=cached; fromCache=true; r.err.clear(); }
    if(body.empty())return r;
    std::string errSave=r.err;
    r=parsePlaces(body);
    r.err=errSave; r.cache=fromCache;
    if(r.v.empty()&&!fromCache&&readFile(cp,cached)){ r=parsePlaces(cached); r.cache=true; }
    return r;
}
static void readPageRef(J& j,Channel& c,std::string& pid,std::string& ptitle,std::string& ctitle){
    if(!j.eat('{'))return;
    std::string k;
    while(objNext(j,k)){
        if(k=="page")readPageRef(j,c,pid,ptitle,ctitle);
        else if(k=="title"){ std::string s=trimStr(j.str()); if(c.title.empty())c.title=s; }
        else if(k=="url"||k=="href"){
            std::string u=j.str();
            if(c.id.empty()&&u.find("/listen/")!=std::string::npos)c.id=lastSeg(u);
        }
        else if(k=="place"){
            if(!j.at('{')){j.skip();continue;}
            if(!j.eat('{'))break;
            std::string pk;
            while(objNext(j,pk)){
                if(pk=="id")pid=j.str();
                else if(pk=="title")ptitle=j.str();
                else j.skip();
            }
            j.eat('}');
        }
        else if(k=="country"){
            if(!j.at('{')){j.skip();continue;}
            if(!j.eat('{'))break;
            std::string ck;
            while(objNext(j,ck)){ if(ck=="title")ctitle=j.str(); else j.skip(); }
            j.eat('}');
        }
        else j.skip();
    }
    j.eat('}');
}
static CityRes fetchCity(const Place& p){
    CityRes r; r.placeId=p.id; r.title=p.title;
    std::string body=httpGet(std::string(RG)+"/ara/content/page/"+p.id+"/channels",r.err,20);
    if(body.empty())body=httpGet(std::string(RG)+"/ara/content/page/"+p.id,r.err,20);
    if(body.empty())return r;
    J j(body);
    if(!j.eat('{')){ r.err="parse"; return r; }
    r.ok=true; r.err.clear();
    std::set<std::string> seen;
    std::string k;
    while(objNext(j,k)){
        if(k!="data"){ j.skip(); continue; }
        if(!j.eat('{'))break;
        std::string dk;
        while(objNext(j,dk)){
            if(dk=="title")r.title=j.str();
            else if(dk=="count")r.count=(int)j.num();
            else if(dk=="utcOffset"){ r.utcOffset=(int)j.num(); r.haveOffset=true; }
            else if(dk=="content"){
                if(!j.eat('['))break;
                while(arrNext(j)){
                    if(!j.eat('{'))break;
                    std::string sk;
                    while(objNext(j,sk)){
                        if(sk!="items"){ j.skip(); continue; }
                        if(!j.eat('['))break;
                        while(arrNext(j)){
                            Channel c; std::string pid,ptitle,ctitle;
                            readPageRef(j,c,pid,ptitle,ctitle);
                            if(!j.ok)break;
                            if(c.id.empty()||c.title.empty()||seen.count(c.id))continue;
                            seen.insert(c.id);
                            c.url=rgStream(c.id);
                            c.placeId=pid.empty()?p.id:pid;
                            c.place=ptitle.empty()?p.title:ptitle;
                            c.country=ctitle.empty()?p.country:ctitle;
                            c.cc=isoForCountry(c.country);
                            if(c.placeId==p.id){ c.lat=p.lat; c.lon=p.lon; }
                            r.ch.push_back(std::move(c));
                        }
                        j.eat(']');
                    }
                    j.eat('}');
                    if(!j.ok)break;
                }
                j.eat(']');
            }
            else j.skip();
        }
        j.eat('}');
    }
    if(r.count<(int)r.ch.size())r.count=(int)r.ch.size();
    return r;
}
static std::vector<SearchHit> fetchSearch(const std::string& q,std::string& err){
    std::vector<SearchHit> out;
    std::string body=httpGet(std::string(RG)+"/search?q="+urlEsc(q),err,18);
    if(body.empty())return out;
    J j(body);
    if(!j.eat('{'))return out;
    std::string k;
    while(objNext(j,k)){
        if(k!="hits"){ j.skip(); continue; }
        if(!j.eat('{'))break;
        std::string hk;
        while(objNext(j,hk)){
            if(hk!="hits"){ j.skip(); continue; }
            if(!j.eat('['))break;
            while(arrNext(j)){
                if(!j.eat('{'))break;
                std::string ik;
                while(objNext(j,ik)){
                    if(ik!="_source"){ j.skip(); continue; }
                    SearchHit h;
                    if(!j.eat('{'))break;
                    std::string sk;
                    while(objNext(j,sk)){
                        if(sk=="code")h.cc=lowerStr(j.str());
                        else if(sk=="type"){ if(j.str()=="place")h.isPlace=true; }
                        else if(sk=="page"){
                            if(!j.eat('{'))break;
                            std::string pk;
                            while(objNext(j,pk)){
                                if(pk=="title")h.title=j.str();
                                else if(pk=="subtitle")h.sub=j.str();
                                else if(pk=="count")h.count=(int)j.num();
                                else if(pk=="url"){
                                    std::string u=j.str(); h.id=lastSeg(u);
                                    if(u.find("/visit/")!=std::string::npos)h.isPlace=true;
                                }
                                else if(pk=="type"){ if(j.str()=="place")h.isPlace=true; }
                                else if(pk=="place"){
                                    if(!j.at('{')){j.skip();continue;}
                                    if(!j.eat('{'))break;
                                    std::string qk;
                                    while(objNext(j,qk)){
                                        if(qk=="id")h.placeId=j.str();
                                        else if(qk=="title")h.place=j.str();
                                        else j.skip();
                                    }
                                    j.eat('}');
                                }
                                else if(pk=="country"){
                                    if(!j.at('{')){j.skip();continue;}
                                    if(!j.eat('{'))break;
                                    std::string ck;
                                    while(objNext(j,ck)){ if(ck=="title")h.country=j.str(); else j.skip(); }
                                    j.eat('}');
                                }
                                else j.skip();
                            }
                            j.eat('}');
                        }
                        else j.skip();
                    }
                    j.eat('}');
                    if(h.cc.empty())h.cc=isoForCountry(h.country);
                    if(h.country.empty()&&h.isPlace)h.country=h.sub;
                    if(!h.id.empty()&&!h.title.empty())out.push_back(std::move(h));
                    if(out.size()>=60)break;
                }
                j.eat(']');
                if(!j.ok||out.size()>=60)break;
            }
            j.eat('}');
        }
        j.eat('}');
    }
    return out;
}
static GeoRes fetchGeo(){
    GeoRes r; std::string e;
    std::string body=httpGet(std::string(RG)+"/geo",e,12);
    if(body.empty())return r;
    J j(body);
    if(!j.eat('{'))return r;
    std::string k;
    while(objNext(j,k)){
        if(k=="latitude")r.lat=(float)j.num();
        else if(k=="longitude")r.lon=(float)j.num();
        else j.skip();
    }
    r.ok=(r.lat!=0||r.lon!=0);
    return r;
}
static void geoArray(J& j,std::vector<std::vector<Vec2>>& out){
    if(!j.eat('['))return;
    if(!arrNext(j)){ j.eat(']'); return; }
    bool isRing=false;
    if(j.at('[')){
        const char* save=j.p;
        j.ws(); ++j.p; j.ws();
        isRing=(j.p<j.e&&*j.p!='['&&*j.p!=']');
        j.p=save;
    }
    if(isRing){
        std::vector<Vec2> line; Vec2 last{1e9f,1e9f};
        do{
            Vec2 p{0,0};
            if(j.eat('[')){
                p.x=(float)j.num();
                if(arrNext(j))p.y=(float)j.num();
                while(arrNext(j))j.skip();
                j.eat(']');
            } else j.skip();
            if(!j.ok)break;
            if(line.empty()||fabsf(p.x-last.x)+fabsf(p.y-last.y)>1.2f){ line.push_back(p); last=p; }
        }while(arrNext(j));
        j.eat(']');
        if(line.size()>2){ line.push_back(line.front()); out.push_back(std::move(line)); }
        return;
    }
    do{ geoArray(j,out); if(!j.ok)break; }while(arrNext(j));
    j.eat(']');
}
static std::vector<std::vector<Vec2>> fetchLand(std::string& err){
    std::string path=gCacheDir+"land.geojson",body;
    if(!readFile(path,body)){
        body=httpGet("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_land.geojson",err,25,false);
        if(body.empty())body=httpGet("https://cdn.jsdelivr.net/gh/nvkelso/natural-earth-vector@master/geojson/ne_110m_land.geojson",err,25,false);
        if(!body.empty())writeFile(path,body);
    }
    std::vector<std::vector<Vec2>> out;
    if(body.empty())return out;
    J j(body);
    if(!j.eat('{'))return out;
    std::string k;
    while(objNext(j,k)){
        if(k!="features"){ j.skip(); continue; }
        if(!j.eat('['))break;
        while(arrNext(j)){
            if(!j.eat('{'))break;
            std::string fk;
            while(objNext(j,fk)){
                if(fk!="geometry"){ j.skip(); continue; }
                if(!j.at('{')){ j.skip(); continue; }
                if(!j.eat('{'))break;
                std::string gk;
                while(objNext(j,gk)){
                    if(gk=="coordinates")geoArray(j,out);
                    else j.skip();
                }
                j.eat('}');
            }
            j.eat('}');
            if(!j.ok)break;
        }
        j.eat(']');
    }
    return out;
}

struct Fav { std::string chId,title,place,placeId,country,url,cc; float lat=0,lon=0; };
struct Preset { bool set=false; std::string chId,title,place,placeId,country,url,cc; float lat=0,lon=0; };
struct State {
    std::vector<Fav> favs; float volume=.8f; Preset presets[6];
    bool uiSounds=true,pauseHiss=true,autoNight=true; std::string lastDay,placesVer;
};
static std::string chObj(const std::string& id,const std::string& title,const std::string& place,
                         const std::string& placeId,const std::string& country,const std::string& url,
                         const std::string& cc,float lat,float lon){
    return "{\"id\":\""+jsonEsc(id)+"\",\"title\":\""+jsonEsc(title)+"\",\"place\":\""+jsonEsc(place)+
           "\",\"placeId\":\""+jsonEsc(placeId)+"\",\"country\":\""+jsonEsc(country)+"\",\"url\":\""+jsonEsc(url)+
           "\",\"cc\":\""+jsonEsc(cc)+"\",\"lat\":"+std::to_string(lat)+",\"lon\":"+std::to_string(lon)+"}";
}
template<class T> static void readCh(J& j,T& f){
    if(!j.eat('{'))return;
    std::string k;
    while(objNext(j,k)){
        if(k=="id")f.chId=j.str();
        else if(k=="title")f.title=j.str();
        else if(k=="place")f.place=j.str();
        else if(k=="placeId")f.placeId=j.str();
        else if(k=="country")f.country=j.str();
        else if(k=="url")f.url=j.str();
        else if(k=="cc")f.cc=j.str();
        else if(k=="lat")f.lat=(float)j.num();
        else if(k=="lon")f.lon=(float)j.num();
        else j.skip();
    }
    j.eat('}');
}
static State loadState(){
    State st; std::string body;
    if(!readFile(gCacheDir+"state.json",body))return st;
    J j(body);
    if(!j.eat('{'))return st;
    std::string k;
    while(objNext(j,k)){
        if(k=="volume")st.volume=(float)j.num();
        else if(k=="uiSounds")st.uiSounds=j.boolv();
        else if(k=="pauseHiss")st.pauseHiss=j.boolv();
        else if(k=="autoNight")st.autoNight=j.boolv();
        else if(k=="lastDay")st.lastDay=j.str();
        else if(k=="placesVer")st.placesVer=j.str();
        else if(k=="favs"){
            if(!j.eat('['))break;
            while(arrNext(j)){ Fav f; readCh(j,f); if(!f.chId.empty())st.favs.push_back(std::move(f)); if(!j.ok)break; }
            j.eat(']');
        }
        else if(k=="presets"){
            if(!j.eat('['))break;
            int i=0;
            while(arrNext(j)){ Preset p; readCh(j,p); if(i<6){ p.set=true; st.presets[i++]=std::move(p); } if(!j.ok)break; }
            j.eat(']');
        }
        else j.skip();
    }
    if(st.volume<=0||st.volume>1)st.volume=.8f;
    for(auto& f:st.favs)if(f.url.empty())f.url=rgStream(f.chId);
    for(auto& p:st.presets)if(p.set&&p.url.empty())p.url=rgStream(p.chId);
    return st;
}
static void saveState(const State& st){
    std::string o="{\"volume\":"+std::to_string(st.volume);
    o+=std::string(",\"uiSounds\":")+(st.uiSounds?"true":"false");
    o+=std::string(",\"pauseHiss\":")+(st.pauseHiss?"true":"false");
    o+=std::string(",\"autoNight\":")+(st.autoNight?"true":"false");
    o+=",\"lastDay\":\""+jsonEsc(st.lastDay)+"\",\"placesVer\":\""+jsonEsc(st.placesVer)+"\"";
    o+=",\"favs\":[";
    for(size_t i=0;i<st.favs.size();i++){ const Fav& f=st.favs[i];
        if(i)o+=",";
        o+=chObj(f.chId,f.title,f.place,f.placeId,f.country,f.url,f.cc,f.lat,f.lon); }
    o+="],\"presets\":[";
    bool first=true;
    for(int i=0;i<6;i++){ const Preset& p=st.presets[i]; if(!p.set)continue;
        if(!first)o+=",";
        first=false;
        o+=chObj(p.chId,p.title,p.place,p.placeId,p.country,p.url,p.cc,p.lat,p.lon); }
    o+="]}";
    writeFile(gCacheDir+"state.json",o);
}
static void loadIdSet(const std::string& file,std::set<std::string>& s){
    std::string b; if(!readFile(gCacheDir+file,b))return;
    size_t i=0;
    while(i<b.size()){ size_t j=b.find('\n',i); if(j==std::string::npos)j=b.size();
        std::string t=trimStr(b.substr(i,j-i)); if(!t.empty())s.insert(t); i=j+1; }
}
static void saveIdSet(const std::string& file,const std::set<std::string>& s){
    std::string o; for(auto& x:s){o+=x;o+='\n';} writeFile(gCacheDir+file,o);
}

static bool decodeImageStb(const std::string& body,int maxDim,std::vector<uint8_t>& out,int& ow,int& oh){
    if(body.size()<16||maxDim<=0)return false;
    int w=0,h=0,ch=0;
    stbi_uc* px=stbi_load_from_memory((const stbi_uc*)body.data(),(int)body.size(),&w,&h,&ch,4);
    if(!px)return false;
    if(w<=0||h<=0||(int64_t)w*h>6000000){stbi_image_free(px);return false;}
    float sc=std::min(1.f,std::min(maxDim/(float)w,maxDim/(float)h));
    ow=std::max(1,(int)(w*sc+.5f));
    oh=std::max(1,(int)(h*sc+.5f));
    out.resize((size_t)ow*oh*4);
    if(ow==w&&oh==h){ memcpy(out.data(),px,(size_t)w*h*4); }
    else{
        for(int y=0;y<oh;y++){
            float sy=(y+.5f)/sc-.5f; if(sy<0)sy=0; if(sy>(float)(h-1))sy=(float)(h-1);
            int y0=(int)sy,y1=std::min(y0+1,h-1); float fy=sy-(float)y0;
            for(int x=0;x<ow;x++){
                float sx=(x+.5f)/sc-.5f; if(sx<0)sx=0; if(sx>(float)(w-1))sx=(float)(w-1);
                int x0=(int)sx,x1=std::min(x0+1,w-1); float fx=sx-(float)x0;
                const uint8_t* p00=px+((size_t)y0*w+x0)*4;
                const uint8_t* p10=px+((size_t)y0*w+x1)*4;
                const uint8_t* p01=px+((size_t)y1*w+x0)*4;
                const uint8_t* p11=px+((size_t)y1*w+x1)*4;
                uint8_t* d=out.data()+((size_t)y*ow+x)*4;
                for(int k=0;k<4;k++){
                    float a=(float)p00[k]+((float)p10[k]-(float)p00[k])*fx;
                    float b=(float)p01[k]+((float)p11[k]-(float)p01[k])*fx;
                    d[k]=(uint8_t)(a+(b-a)*fy+.5f);
                }
            }
        }
    }
    stbi_image_free(px);
    return true;
}
static bool decodePNG(const std::string& body,int& w,int& h,std::vector<uint8_t>& rgba){
	    if(decodeImageStb(body,4096,rgba,w,h))return true;
    const AVCodec* dec=avcodec_find_decoder(AV_CODEC_ID_PNG); if(!dec)return false;
    AVCodecContext* ctx=avcodec_alloc_context3(dec); if(!ctx)return false;
    bool ok=false;
    if(avcodec_open2(ctx,dec,nullptr)>=0){
        std::vector<uint8_t> buf(body.begin(),body.end());
        buf.resize(buf.size()+AV_INPUT_BUFFER_PADDING_SIZE,0);
        AVPacket* pkt=av_packet_alloc(); AVFrame* fr=av_frame_alloc();
        if(pkt&&fr){
            pkt->data=buf.data(); pkt->size=(int)body.size();
            if(avcodec_send_packet(ctx,pkt)>=0&&avcodec_receive_frame(ctx,fr)>=0&&fr->width>0&&fr->height>0){
                w=fr->width; h=fr->height;
                SwsContext* sws=sws_getContext(w,h,(AVPixelFormat)fr->format,w,h,AV_PIX_FMT_RGBA,SWS_BILINEAR,nullptr,nullptr,nullptr);
                if(sws){ rgba.resize((size_t)w*h*4);
                    uint8_t* d[4]={rgba.data(),nullptr,nullptr,nullptr}; int ds[4]={w*4,0,0,0};
                    sws_scale(sws,fr->data,fr->linesize,0,h,d,ds); sws_freeContext(sws); ok=true; }
            }
        }
        if(fr)av_frame_free(&fr);
        if(pkt)av_packet_free(&pkt);
    }
    avcodec_free_context(&ctx);
    return ok;
}

struct Sfx {
    enum { TICK=1,POP=2,BACK=3,FAV=4,SWEEP=6,PAGE=7,CHIME=8,STAMP=9 };
    std::atomic<bool> uiOn{true},hissOn{true},muted{false},tuning{false},paused{false};
    std::atomic<int> req{0};
    struct V { int type=0; float t=0,dur=.1f,g=.2f,ph=0; };
    V v[6]; uint32_t ns=0x9E3779B9u; float lp=0,lp2=0,lp3=0;
    float rnd(){ ns^=ns<<13; ns^=ns>>17; ns^=ns<<5; return (ns/4294967296.f)*2.f-1.f; }
    void trigger(int t){ if(uiOn.load())req.store(t); }
    void mix(int16_t* o,int frames,int rate){
        if(muted.load()||frames<=0)return;
        int r=req.exchange(0);
        if(r){
            int slot=-1;
            for(int i=0;i<6;i++)if(!v[i].type){slot=i;break;}
            if(slot<0){ float best=-1; for(int i=0;i<6;i++){ float p=v[i].t/v[i].dur; if(p>best){best=p;slot=i;} } }
            V& n=v[slot]; n.type=r; n.t=0; n.ph=0;
            n.dur=r==TICK?.05f:r==POP?.12f:r==BACK?.11f:r==FAV?.26f:r==PAGE?.32f:r==CHIME?.5f:r==STAMP?.26f:.5f;
            n.g  =r==TICK?.12f:r==POP?.18f:r==BACK?.15f:r==FAV?.17f:r==PAGE?.12f:r==CHIME?.12f:r==STAMP?.22f:.14f;
        }
        float dt=1.f/rate;
        bool tune=tuning.load()&&!paused.load();
        bool hiss=hissOn.load()&&paused.load();
        for(int i=0;i<frames;i++){
            float s=0;
            for(int k=0;k<6;k++){
                V& a=v[k]; if(!a.type)continue;
                float p=a.t/a.dur; if(p>=1.f){a.type=0;continue;}
                float atk=clampf(p/.06f,0.f,1.f);
                float e=atk*expf(-p*5.5f);
                switch(a.type){
                case TICK:
                    s+=sinf(2*(float)M_PI*1700.f*a.t)*expf(-p*8.f)*atk*a.g*.7f;
                    s+=sinf(2*(float)M_PI*850.f*a.t)*expf(-p*12.f)*atk*a.g*.3f;
                    break;
                case POP: {
                    a.ph+=2*(float)M_PI*(420.f+430.f*(1.f-expf(-p*6.f)))*dt;
                    float e2=sinf((float)M_PI*clampf(p*1.05f,0.f,1.f));
                    s+=sinf(a.ph)*e2*a.g;
                    s+=sinf(2.f*a.ph)*e2*e2*a.g*.18f;
                } break;
                case BACK: {
                    a.ph+=2*(float)M_PI*(520.f-260.f*p)*dt;
                    s+=sinf(a.ph)*e*a.g;
                    s+=sinf(2.f*a.ph)*e*e*a.g*.15f;
                } break;
                case FAV: {
                    float e1=expf(-p*6.f)*atk;
                    float q=clampf((p-.28f)/.72f,0.f,1.f);
                    float e2=p>.28f?expf(-q*5.f)*clampf(q/.1f,0.f,1.f):0.f;
                    s+=sinf(2*(float)M_PI*659.25f*a.t)*e1*a.g*.8f;
                    s+=sinf(2*(float)M_PI*880.f*a.t)*e2*a.g*.9f;
                } break;
                case PAGE: {
                    float n2=rnd();
                    lp2+=(.42f-.20f*p)*(n2-lp2);
                    lp3+=.05f*(n2-lp3);
                    float band=lp2-lp3;
                    float e2=sinf((float)M_PI*powf(p,.75f)); e2*=e2;
                    float fl=.8f+.2f*sinf(p*31.f+1.3f);
                    s+=band*e2*fl*a.g*3.2f;
                } break;
                case CHIME: {
                    float e1=expf(-p*4.5f)*atk;
                    float q=clampf((p-.18f)/.82f,0.f,1.f);
                    float e2=p>.18f?expf(-q*4.f)*clampf(q/.08f,0.f,1.f):0.f;
                    s+=(sinf(2*(float)M_PI*659.25f*a.t)*e1+sinf(2*(float)M_PI*987.77f*a.t)*e2*.7f)*a.g;
                    s+=sinf(2*(float)M_PI*1318.5f*a.t)*e1*e1*a.g*.12f;
                } break;
                case STAMP: {
                    float e1=expf(-p*7.f)*atk;
                    a.ph+=2*(float)M_PI*(190.f-90.f*p)*dt;
                    float th=sinf(a.ph)*e1;
                    float wood=(sinf(2*(float)M_PI*320.f*a.t)+.6f*sinf(2*(float)M_PI*480.f*a.t))*expf(-p*11.f)*atk;
                    float n2=rnd(); lp2+=.3f*(n2-lp2);
                    s+=(th*.8f+wood*.3f+(n2-lp2)*expf(-p*16.f)*.4f)*a.g;
                } break;
                default: {
                    float e2=sinf((float)M_PI*p);
                    float n2=rnd(); lp+=(.08f+.30f*(1.f-p))*(n2-lp);
                    s+=(n2-lp)*e2*a.g*1.2f;
                    a.ph+=2*(float)M_PI*(700.f-450.f*p)*dt;
                    s+=sinf(a.ph)*e2*e2*a.g*.25f;
                } break;
                }
                a.t+=dt;
            }
            if(tune){ float n2=rnd(); lp+=.10f*(n2-lp); s+=(n2-lp)*.06f; }
            if(hiss)s+=rnd()*.005f;
            if(s!=0.f){
                int add=(int)(s*32767.f*.55f);
                for(int c=0;c<2;c++){ int idx=i*2+c,cur=o[idx]+add;
                    o[idx]=(int16_t)(cur>32767?32767:(cur<-32768?-32768:cur)); }
            }
        }
    }
};


struct Player {
    enum St { STOPPED,BUFFERING,PLAYING,PAUSED,FAILED };
    static constexpr int RATE=48000, CH=2,N=1024;
    std::vector<uint8_t> ring; size_t head=0,tail=0,used=0;
    std::vector<int16_t> mon; size_t monPos=0;
    SDL_AudioDeviceID dev=0; std::thread th; Sfx* sfx=nullptr;
    std::atomic<bool> run{false},paused{false},buffering{true};
    std::atomic<float> volume{.8f};
    std::atomic<int> state{STOPPED};
    std::atomic<int64_t> deadline{0};
    std::mutex metaMtx; std::string track,lastErr;
    std::vector<std::pair<std::string,std::string>> history;
    std::vector<std::string> cands;
    std::vector<int64_t> candOffsets;
    std::vector<int64_t> candSeekSec;
    std::atomic<bool> liveOn{false};
    std::atomic<int64_t> liveDur{0}, livePhase{0};
    static int intCB(void* op){
        Player* p=(Player*)op;
        if(!p->run.load()||gQuit.load())return 1;
        int64_t d=p->deadline.load();
        return (d>0&&av_gettime_relative()>d)?1:0;
    }
    void start(const std::string& u){
        std::vector<std::string> c;c.push_back(u);
        if(u.rfind("https://",0)==0)c.push_back("http://"+u.substr(8));
        startList(std::move(c));
    }
    void startList(std::vector<std::string> urls,
                   std::vector<int64_t> offs={}, std::vector<int64_t> secs={}){
        stop();
        for(size_t i=0,n=urls.size();i<n;i++)
            if(urls[i].rfind("https://",0)==0)urls.push_back("http://"+urls[i].substr(8));
        cands=std::move(urls); candOffsets=std::move(offs); candSeekSec=std::move(secs);
        if(dev)SDL_LockAudioDevice(dev);
        ring.assign((size_t)RATE*CH*2*4,0);head=tail=used=0;
        mon.assign(N,0);monPos=0;
        if(dev)SDL_UnlockAudioDevice(dev);
        buffering=true;paused=false;state=BUFFERING;
        if(sfx){sfx->tuning=true;sfx->paused=false;sfx->trigger(Sfx::SWEEP);}
        {std::lock_guard<std::mutex> lk(metaMtx);track.clear();lastErr.clear();history.clear();}
        run=true;th=std::thread([this]{ try{ loop(); }catch(...){ state=FAILED; buffering=false; } });
    }
    void stop(){
        run=false; deadline.store(0);
        if(th.joinable())th.join();
        state=STOPPED; buffering=true;
        if(sfx){ sfx->tuning=false; sfx->paused=false; }
    }
    void togglePause(){
        if(state==STOPPED||state==FAILED)return;
        paused=!paused;
        if(sfx){ sfx->paused=paused.load(); sfx->trigger(Sfx::POP); }
        state=paused?PAUSED:(buffering?BUFFERING:PLAYING);
    }
    std::string trackTitle(){ std::lock_guard<std::mutex> lk(metaMtx); return track; }
    std::string error(){ std::lock_guard<std::mutex> lk(metaMtx); return lastErr; }
    std::vector<std::pair<std::string,std::string>> hist(){ std::lock_guard<std::mutex> lk(metaMtx); return history; }
    void snapshot(std::vector<int16_t>& out){
        out.resize(N);
        if(dev)SDL_LockAudioDevice(dev);
        if(mon.empty())for(auto& v:out)v=0;
        else for(int i=0;i<N;i++)out[i]=mon[(monPos+(size_t)i)%mon.size()];
        if(dev)SDL_UnlockAudioDevice(dev);
    }
    void pushTrack(const std::string& s){
        std::lock_guard<std::mutex> lk(metaMtx);
        track=s;
        if(!history.empty()&&history.front().first==s)return;
        tm g=tmLocal(time(nullptr)); char tb[8]; snprintf(tb,8,"%02d:%02d",g.tm_hour,g.tm_min);
        history.insert(history.begin(),std::make_pair(s,std::string(tb)));
        if(history.size()>12)history.resize(12);
    }
private:
    void setErr(const std::string& s){ std::lock_guard<std::mutex> lk(metaMtx); lastErr=s; }
    void push(const uint8_t* d,int n){
        if(!dev){ buffering=false; if(state==BUFFERING)state=PLAYING; if(sfx)sfx->tuning=false; return; }
        while(n>0&&run){
            size_t cap=0,u=0;
            SDL_LockAudioDevice(dev);
            cap=ring.size();
            if(!cap){ SDL_UnlockAudioDevice(dev); return; }
            int k=std::min((int)(cap-used),n);
            for(int i=0;i<k;i++){ ring[head]=d[i]; head=(head+1)%cap; }
            used+=(size_t)k; u=used;
            SDL_UnlockAudioDevice(dev);
            d+=k; n-=k;
            if(u>0&&!paused){ buffering=false; if(state==BUFFERING)state=PLAYING; if(sfx)sfx->tuning=false; }
            if(n>0)SDL_Delay(10);
        }
    }
    bool retry(int f){
        if(f>5){ state=FAILED; if(sfx)sfx->tuning=false; return false; }
        for(int i=0;i<15&&run;i++)SDL_Delay(60);
        return run;
    }
    void liveSeek(AVFormatContext* fmt,AVCodecContext* ctx,int64_t sec){
        if(!fmt||!fmt->pb||sec<=0)return;
        int64_t br=ctx->bit_rate>0?ctx->bit_rate:128000;
        int64_t sz=avio_size(fmt->pb);
        int64_t byte=sec*br/8;
        if(sz>0){ if(byte>sz-65536)byte=sz-65536; }
        if(byte<=0)return;
        deadline.store(av_gettime_relative()+4000000);
        if(avio_seek(fmt->pb,byte,SEEK_SET)>=0){
            avformat_flush(fmt); avcodec_flush_buffers(ctx);
        }
        deadline.store(0);
    }
    void loop(){
        int fails=0;
        while(run){
            AVFormatContext* fmt=nullptr; bool opened=false;
            int64_t openOff=0, openSec=0;
            for(size_t ci=0;ci<cands.size()&&run;ci++){
                const std::string cu=cands[ci];
                int64_t off0=ci<candOffsets.size()?candOffsets[ci]:0;
                int64_t sec0=ci<candSeekSec.size()?candSeekSec[ci]:0;
                fmt=avformat_alloc_context(); if(!fmt)break;
                fmt->interrupt_callback.callback=&Player::intCB;
                fmt->interrupt_callback.opaque=this;
                const bool isFile=cu.find(".mp3")!=std::string::npos;
                const bool isArch=cu.find("archive.org")!=std::string::npos;
                const AVInputFormat* ifmt=isFile?av_find_input_format("mp3"):nullptr;
                AVDictionary* opt=nullptr;
                av_dict_set(&opt,"icy",isFile?"0":"1",0);
                av_dict_set(&opt,"rw_timeout","4000000",0);
                av_dict_set(&opt,"timeout","4000000",0);
                av_dict_set(&opt,"reconnect","1",0);
                av_dict_set(&opt,"reconnect_streamed","1",0);
                av_dict_set(&opt,"reconnect_delay_max","2",0);
                av_dict_set(&opt,"multiple_requests","1",0);
                av_dict_set(&opt,"tls_verify","0",0);
                av_dict_set(&opt,"user_agent",UA_BROWSER,0);
                if(isFile){
                    av_dict_set(&opt,"probesize","32768",0);
                    av_dict_set(&opt,"analyzeduration","0",0);
                } else {
                    av_dict_set(&opt,"headers","Referer: https://radio.garden/\r\n",0);
                }
                if(off0>0){
                    av_dict_set(&opt,"offset",std::to_string(off0).c_str(),0);
                    av_dict_set(&opt,"seekable","0",0);
                }
                deadline.store(av_gettime_relative()+(isArch?6000000:10000000));
                int r=avformat_open_input(&fmt,cu.c_str(),ifmt,&opt);
                deadline.store(0); av_dict_free(&opt);
                if(r>=0){opened=true;openOff=off0;openSec=sec0;break;}
                char eb[256]; av_strerror(r,eb,sizeof eb); setErr(eb); fmt=nullptr;
            }
            if(!opened||!fmt){ if(!retry(++fails))break; continue; }
            if(avformat_find_stream_info(fmt,nullptr)<0){
                setErr("no stream info"); avformat_close_input(&fmt);
                if(!retry(++fails))break;
                continue;
            }
            const AVCodec* dec=nullptr;
            int si=av_find_best_stream(fmt,AVMEDIA_TYPE_AUDIO,-1,-1,&dec,0);
            if(si<0||!dec){ setErr("no audio"); avformat_close_input(&fmt); if(!retry(++fails))break; continue; }
            AVCodecContext* ctx=avcodec_alloc_context3(dec);
            if(!ctx){ setErr("ctx alloc"); avformat_close_input(&fmt); if(!retry(++fails))break; continue; }
            avcodec_parameters_to_context(ctx,fmt->streams[si]->codecpar);
            if(avcodec_open2(ctx,dec,nullptr)<0){
                setErr("codec open"); avcodec_free_context(&ctx); avformat_close_input(&fmt);
                if(!retry(++fails))break; continue;
            }
            SwrContext* swr=swr_alloc();
            if(!swr){ setErr("swr alloc"); avcodec_free_context(&ctx); avformat_close_input(&fmt); if(!retry(++fails))break; continue; }
#if LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(57,28,100)
            AVChannelLayout st=AV_CHANNEL_LAYOUT_STEREO;
            av_opt_set_chlayout(swr,"in_chlayout",&ctx->ch_layout,0);
            av_opt_set_chlayout(swr,"out_chlayout",&st,0);
#else
            av_opt_set_channel_layout(swr,"in_channel_layout",
                ctx->channel_layout?ctx->channel_layout:av_get_default_channel_layout(ctx->channels),0);
            av_opt_set_channel_layout(swr,"out_channel_layout",AV_CH_LAYOUT_STEREO,0);
#endif
            av_opt_set_int(swr,"in_sample_rate",ctx->sample_rate,0);
            av_opt_set_sample_fmt(swr,"in_sample_fmt",ctx->sample_fmt,0);
            av_opt_set_int(swr,"out_sample_rate",RATE,0);
            av_opt_set_sample_fmt(swr,"out_sample_fmt",AV_SAMPLE_FMT_S16,0);
            if(swr_init(swr)<0){
                setErr("swr init"); swr_free(&swr); avcodec_free_context(&ctx); avformat_close_input(&fmt);
                if(!retry(++fails))break; continue;
            }
            AVPacket* pkt=av_packet_alloc(); AVFrame* fr=av_frame_alloc();
            if(!pkt||!fr){
                if(pkt)av_packet_free(&pkt);
                if(fr)av_frame_free(&fr);
                setErr("frame alloc"); swr_free(&swr); avcodec_free_context(&ctx); avformat_close_input(&fmt);
                if(!retry(++fails))break; continue;
            }
            std::vector<uint8_t> out((size_t)RATE*CH*2);
            fails=0; int mt=0; setErr("");
            if(openOff==0&&openSec>0)liveSeek(fmt,ctx,openSec);
            bool wasPaused=false;
            while(run){
                if(paused){wasPaused=true;SDL_Delay(40);continue;}
                if(wasPaused){
                    wasPaused=false;
                    if(liveOn.load()&&liveDur.load()>0&&openOff==0){
                        int64_t d=liveDur.load();
                        int64_t n=(int64_t)time(nullptr)+livePhase.load();
                        liveSeek(fmt,ctx,((n%d)+d)%d);
                    }
                }
                deadline.store(av_gettime_relative()+12000000);
                if(av_read_frame(fmt,pkt)<0){ av_packet_unref(pkt); break; }
                deadline.store(0);
                if(pkt->stream_index==si){
                    avcodec_send_packet(ctx,pkt);
                    while(avcodec_receive_frame(ctx,fr)==0){
                        uint8_t* d=out.data();
                        int n=swr_convert(swr,&d,RATE,(const uint8_t**)fr->extended_data,fr->nb_samples);
                        if(n>0)push(out.data(),n*CH*2);
                    }
                }
                av_packet_unref(pkt);
                if(++mt>=48){
                    mt=0;
                    AVDictionaryEntry* e=av_dict_get(fmt->metadata,"StreamTitle",nullptr,0);
                    if(e&&e->value&&e->value[0]){
                        std::string s=trimStr(e->value);
                        if(!s.empty()){ { std::lock_guard<std::mutex> lk(metaMtx); track=s; } pushTrack(s); }
                    }
                }
            }
            deadline.store(0);
            av_packet_free(&pkt); av_frame_free(&fr);
            swr_free(&swr); avcodec_free_context(&ctx); avformat_close_input(&fmt);
            if(run&&!paused){ buffering=true; state=BUFFERING; if(sfx)sfx->tuning=true; if(!retry(++fails))break; }
        }
        if(run){ state=FAILED; if(sfx)sfx->tuning=false; }
    }
};

static void gtaSfxMix(int16_t* o,int frames);
struct Audio {
    Player* player=nullptr; Sfx sfx; SDL_AudioDeviceID dev=0;
    std::atomic<float> fade{1.f};
    static void SDLCALL cb(void* ud,Uint8* out,int len){
        Audio* a=(Audio*)ud; Player* p=a->player;
        if(!p||len<2){ if(len>0)SDL_memset(out,0,len); return; }
        int16_t* o=(int16_t*)out; int n=len/2;
        bool mute=a->sfx.muted.load();
        float vol=mute?0.f:p->volume.load()*a->fade.load();
        bool pull=p->run.load()&&!p->paused.load()&&!mute&&!p->ring.empty();
        int32_t ml=0;
        for(int i=0;i<n;i++){
            int16_t s=0;
            if(pull&&p->used>=2){
                s=(int16_t)(p->ring[p->tail]|(p->ring[p->tail+1]<<8));
                p->tail=(p->tail+2)%p->ring.size(); p->used-=2;
            }
            // монитор: (L+R)/2 на 24 кГц — точная база для FFT
            if((i&1)==0)ml=s;
            else if(!p->mon.empty()){
                p->mon[p->monPos]=(int16_t)((ml+(int32_t)s)>>1);
                p->monPos=(p->monPos+1)%p->mon.size();
            }
            o[i]=(int16_t)(s*vol);
        }
        a->sfx.mix(o,n/2,Player::RATE);
        if(!mute)gtaSfxMix(o,n/2);
    }
    void open(Player* p){
        player=p; p->sfx=&sfx;
        SDL_AudioSpec w{},g{};
        w.freq=Player::RATE; w.format=AUDIO_S16SYS; w.channels=2; w.samples=2048;
        w.callback=&Audio::cb; w.userdata=this;
        dev=SDL_OpenAudioDevice(nullptr,0,&w,&g,0); p->dev=dev;
        if(dev)SDL_PauseAudioDevice(dev,0);
        else SDL_Log("audio: %s",SDL_GetError());
    }
    void close(){ if(dev){ SDL_CloseAudioDevice(dev); dev=0; if(player)player->dev=0; } }
};

struct Globe {
    float yaw=.5f,pitch=.45f,yawV=0,pitchV=0,R=285,targetR=285;
    Vec2 center{430,372},centerHome{430,372},centerZen{640,348};
    bool zen=false;
    std::vector<Place>* places=nullptr;
    std::vector<Vec3> pv; std::vector<Vec2> pp;
    std::vector<float> pz,dotR,lit;
    std::vector<int> order;
    std::vector<std::vector<Vec3>> landv,gridv;
    std::vector<uint8_t> cells;
    static constexpr int LITN=64;
    static constexpr int HALON=8;
    std::vector<SDL_Rect> core[LITN],halo1[HALON],halo2[HALON];
    int nearest=-1,selected=-1; float nearestZ=0;
    bool selPt=false; Vec3 selV{0,0,0}; Vec2 selP{0,0}; float selZ=-1.f;
    float idle=0,flyYaw=0,flyPitch=0,zoomIdle=0,sunTimer=0,selGlow=0;
    bool dragging=false,flying=false,locked=false,moving=false;
    float dotFade=1.f;
    float cy=1,sy=0,cp=1,sp=0;
    static constexpr float DEF_R=285.f;
    static constexpr float HOME_R=980.f, HOME_CX=640.f, HOME_CY=1450.f;
    bool home=false;
    Vec3 sunV{0,0,1},sunBase{0,0,1},litSun{9,9,9};
    float cYaw=1e9f,cPitch=1e9f,cR=-1,cCx=1e9f,cCy=1e9f; bool projDirty=true;
    SDL_Texture* cache=nullptr; bool cacheValid=false;
    SDL_Texture* sph=nullptr; bool sphValid=false;
    float kYaw=1e9f,kPitch=1e9f,kR=-1,kWarm=-1,sR=-1,sWarm=-1,kCx=1e9f,kCy=1e9f;
    Vec3 kSun{0,0,0};
    int rev=0,kRev=-1;
    int scrW=1280,scrH=720;
    struct Spark { float x=0,y=0,vx=0,vy=0,life=0,max=1,sz=1,rot=0,rv=0; int kind=0; };
    Spark spk[36];
    bool stOn=false; float stT=0,stDur=5.f,stNext=26.f,stEmit=0,stAng=0;
    Vec2 stP0{},stP1{},stC{};
    Globe(){
        cells.resize(640*360);
        for(int k=0;k<LITN;k++)core[k].reserve(256);
        for(int k=0;k<HALON;k++){halo1[k].reserve(256);halo2[k].reserve(256);}
        stNext=frand(20.f,36.f);
    }
    float rimR() const { return R*1.012f; }
    float inputK() const { return clampf(DEF_R/R,.12f,1.f); }
    void invalidate(){ rev++; cacheValid=false; projDirty=true; }
    void initCache(SDL_Renderer* r){
        if(!SDL_RenderTargetSupported(r))return;
        cache=SDL_CreateTexture(r,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,1280,720);
        sph  =SDL_CreateTexture(r,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,1280,720);
        if(cache)SDL_SetTextureBlendMode(cache,SDL_BLENDMODE_BLEND);
        if(sph)SDL_SetTextureBlendMode(sph,SDL_BLENDMODE_BLEND);
    }
    void destroyCache(){
        if(cache){SDL_DestroyTexture(cache);cache=nullptr;}
        if(sph){SDL_DestroyTexture(sph);sph=nullptr;}
        cacheValid=sphValid=false;
    }
    SDL_Rect bbox(int W,int H) const {
        float pad=R*1.34f+8.f;
        int x0=(int)floorf(center.x-pad),y0=(int)floorf(center.y-pad);
        int x1=(int)ceilf(center.x+pad),y1=(int)ceilf(center.y+pad);
        if(x0<0)x0=0; if(y0<0)y0=0; if(x1>W)x1=W; if(y1>H)y1=H;
        if(x1<=x0||y1<=y0)return SDL_Rect{0,0,0,0};
        return SDL_Rect{x0,y0,x1-x0,y1-y0};
    }
    void buildGrid(){
        if(!gridv.empty())return;
        for(int la=-60;la<=60;la+=30){ std::vector<Vec3> v;
            for(int lo=-180;lo<=180;lo+=10)v.push_back(ll2v((float)lo,(float)la)); gridv.push_back(std::move(v)); }
        for(int lo=-180;lo<180;lo+=30){ std::vector<Vec3> v;
            for(int la=-80;la<=80;la+=10)v.push_back(ll2v((float)lo,(float)la)); gridv.push_back(std::move(v)); }
        invalidate();
    }
    void buildPlaces(){
        pv.clear(); pp.clear(); pz.clear(); order.clear(); dotR.clear(); lit.clear();
        nearest=-1; selected=-1; nearestZ=0;
        if(!places)return;
        size_t n=places->size();
        pv.resize(n); pp.resize(n); pz.resize(n); dotR.resize(n); order.resize(n);
        lit.assign(n,1.f); litSun=Vec3{9,9,9};
        for(size_t i=0;i<n;i++){
            pv[i]=ll2v((*places)[i].lon,(*places)[i].lat); order[i]=(int)i;
            float br=.8f+log2f(1+(float)(*places)[i].size)*.22f;
            dotR[i]=br>2.6f?2.6f:br;
        }
        std::sort(order.begin(),order.end(),[&](int a,int b){return (*places)[a].size>(*places)[b].size;});
        invalidate();
    }
    void buildLand(const std::vector<std::vector<Vec2>>& land){
        landv.clear();
        for(auto& r:land){
            std::vector<Vec3> v; v.reserve(r.size());
            for(auto& g:r)v.push_back(ll2v(g.x,g.y));
            landv.push_back(std::move(v));
        }
        buildGrid(); invalidate();
    }
    void setSelected(const std::string& id){
        selected=-1;
        if(id.empty()||!places)return;
        for(size_t i=0;i<places->size();i++)
            if((*places)[i].id==id){selected=(int)i;break;}
    }
    void setSelPoint(float lat,float lon,bool on){
        selPt=on; selZ=-1.f;
        if(on)selV=ll2v(lon,lat);
    }
    void updRot(){ cy=cosf(yaw);sy=sinf(yaw);cp=cosf(pitch);sp=sinf(pitch); }
    Vec3 rotVec(const Vec3& v) const {
        float x1=v.x*cy+v.z*sy,z1=-v.x*sy+v.z*cy;
        return {x1,v.y*cp-z1*sp,v.y*sp+z1*cp};
    }
    Vec2 proj(const Vec3& p) const { return {center.x+R*p.x,center.y-R*p.y}; }
    Vec2 projClamp(Vec3 p) const {
        if(p.z<0.f){
            float l=sqrtf(p.x*p.x+p.y*p.y);
            if(l>1e-5f){ p.x/=l; p.y/=l; }
            p.z=0.f;
        }
        return proj(p);
    }
    void drag(float dx,float dy){
        if(locked||home)return;
        flying=false; idle=0; zoomIdle=0;
        float k=inputK();
        yaw+=dx*.0032f*k; pitch=clampf(pitch+dy*.0026f*k,-1.35f,1.35f);
        yawV=lerpf(yawV,dx*.16f*k,.5f); pitchV=lerpf(pitchV,dy*.13f*k,.5f);
    }
    void flyTo(float lat,float lon){
        if(locked)return;
        flying=true; idle=0; zoomIdle=0; yawV=pitchV=0;
        flyYaw=yaw+wrapPi(-lon*DEG-yaw); flyPitch=clampf(lat*DEG,-1.35f,1.35f);
    }
    void zoom(float d){ if(locked||zen||home)return; zoomIdle=0; targetR=clampf(targetR*expf(clampf(d,-60,60)*.006f),150,2400); }
    void zoomHold(float dir,float dt){ if(locked||zen||home)return; zoomIdle=0; targetR=clampf(targetR*expf(dir*1.35f*dt),150,2400); }
    void updateSun(float dt){
        sunTimer-=dt;
        if(sunTimer<=0){
            sunTimer=1.f;
            tm g=tmUtc(time(nullptr));
            float h=g.tm_hour+g.tm_min/60.f;
            sunBase=ll2v((12.f-h)*15.f,23.44f*sinf(2*(float)M_PI*(g.tm_yday-81)/365.f));
        }
        sunV=rotVec(sunBase);
    }
    void updateLit(){
        if(pv.empty())return;
        if(lit.size()!=pv.size())lit.assign(pv.size(),1.f);
        if(litSun.x==sunBase.x&&litSun.y==sunBase.y&&litSun.z==sunBase.z)return;
        litSun=sunBase;
        for(size_t i=0;i<pv.size();i++)
            lit[i]=pv[i].x*sunBase.x+pv[i].y*sunBase.y+pv[i].z*sunBase.z;
    }
    Vec2 starAt(float u) const {
        float m=1.f-u;
        return {m*m*stP0.x+2*m*u*stC.x+u*u*stP1.x,
                m*m*stP0.y+2*m*u*stC.y+u*u*stP1.y};
    }
    void starSpawn(){
        float Wf=(float)scrW,Hf=(float)scrH;
        float side=frand(0,1)<.5f?1.f:-1.f;
        float sx=side>0?frand(-170.f,Wf*.30f):frand(Wf*.70f,Wf+170.f);
        float sy=frand(-90.f,Hf*.32f);
        float a=side>0?frand(.20f,.50f):((float)M_PI-frand(.20f,.50f));
        float dist=frand(.88f,1.14f)*Wf;
        stP0={sx,sy};
        stP1={sx+cosf(a)*dist,sy+sinf(a)*dist};
        float mx=(stP0.x+stP1.x)*.5f,my=(stP0.y+stP1.y)*.5f;
        float px=-(stP1.y-stP0.y),py=(stP1.x-stP0.x);
        float pl=sqrtf(px*px+py*py);
        if(pl>1e-3f){px/=pl;py/=pl;}
        float bow=frand(-.05f,.05f)*dist;
        stC={mx+px*bow,my+py*bow};
        stAng=a; stDur=frand(4.2f,5.8f); stT=0; stEmit=0; stOn=true;
    }
    void starUpdate(float dt){
        for(int i=0;i<36;i++){
            Spark& p=spk[i];
            if(p.life<=0)continue;
            p.life-=dt;
            if(p.life<=0){p.life=0;continue;}
            p.x+=p.vx*dt; p.y+=p.vy*dt;
            float dmp=powf(.30f,dt);
            p.vx*=dmp; p.vy*=dmp; p.vy+=7.f*dt;
            p.rot+=p.rv*dt;
        }
        if(!stOn){
            stNext-=dt;
            if(stNext<=0)starSpawn();
            return;
        }
        stT+=dt;
        float u=stT/stDur;
        if(u>=1.f){ stOn=false; stNext=frand(120.f,240.f); return; }
        stEmit-=dt;
        if(stEmit<=0&&u>.06f&&u<.86f){
            stEmit=frand(.055f,.10f);
            Vec2 h=starAt(clampf(u,0.f,1.f));
            int n=frand(0,1)<.22f?2:1;
            for(int q=0;q<n;q++){
                int slot=-1;
                for(int i=0;i<36;i++)if(spk[i].life<=0){slot=i;break;}
                if(slot<0)break;
                Spark& p=spk[slot];
                p.x=h.x+frand(-3.5f,3.5f); p.y=h.y+frand(-3.5f,3.5f);
                float bx=-cosf(stAng)*frand(8.f,26.f),by=-sinf(stAng)*frand(8.f,26.f);
                p.vx=bx+frand(-14.f,14.f); p.vy=by+frand(-14.f,14.f);
                p.max=frand(.85f,1.55f); p.life=p.max;
                p.sz=frand(1.5f,3.4f);
                p.rot=frand(0.f,2*(float)M_PI); p.rv=frand(-1.5f,1.5f);
                p.kind=frand(0,1)<.72f?0:1;
            }
        }
    }
    void starDraw(SDL_Renderer* r,Batch& b,float t){
        (void)r;
        for(int i=0;i<36;i++){
            const Spark& p=spk[i];
            if(p.life<=0)continue;
            float a=p.life/p.max,k=1.f-a;
            float pop=k<.16f?k/.16f:1.f;
            float al=clampf(pop*a*a,0.f,1.f);
            if(al<=.01f)continue;
            SDL_Color col=mixc(C(255,252,242),C(255,196,116),clampf(k*1.15f,0.f,1.f));
            float sz=p.sz*(.55f+.45f*a);
            if(p.kind==0){
                float c=cosf(p.rot),s=sinf(p.rot);
                float L=sz*2.7f,Wd=sz*.40f,L2=sz*1.45f;
                int A=(int)(215*al);
                b.quad(p.x+c*L,p.y+s*L, p.x-s*Wd,p.y+c*Wd, p.x-c*L,p.y-s*L, p.x+s*Wd,p.y-c*Wd, alpha(col,A));
                b.quad(p.x-s*L2,p.y+c*L2, p.x-c*Wd,p.y-s*Wd, p.x+s*L2,p.y-c*L2, p.x+c*Wd,p.y+s*Wd, alpha(col,(int)(A*.82f)));
                b.circle(p.x,p.y,sz*.42f,alpha(C(255,255,255),(int)(190*al)),6);
            } else b.circle(p.x,p.y,sz*.55f,alpha(col,(int)(170*al)),6);
        }
        if(!stOn)return;
        float u=clampf(stT/stDur,0.f,1.f);
        float env=1.f;
        if(u<.10f)env=u/.10f;
        else if(u>.78f)env=(1.f-u)/.22f;
        env=clampf(env,0.f,1.f); env=env*env*(3.f-2.f*env);
        if(env<=.005f)return;
        const int N=20; const float TU=.19f;
        Vec2 pts[N+1]; float ff[N+1];
        for(int i=0;i<=N;i++){
            float ui=u-TU*i/(float)N; if(ui<0.f)ui=0.f;
            pts[i]=starAt(ui); ff[i]=1.f-i/(float)N;
        }
        for(int i=0;i<N;i++){
            float dx=pts[i+1].x-pts[i].x,dy=pts[i+1].y-pts[i].y;
            float l=sqrtf(dx*dx+dy*dy); if(l<1e-3f)continue;
            float nx=-dy/l,ny=dx/l;
            float w0=(2.5f*powf(ff[i],.75f)+.35f)*3.4f;
            float w1=(2.5f*powf(ff[i+1],.75f)+.35f)*3.4f;
            SDL_Color c0=mixc(C(255,255,255),C(255,178,96),1.f-ff[i]);
            SDL_Color c1=mixc(C(255,255,255),C(255,178,96),1.f-ff[i+1]);
            int a0=(int)(46*env*powf(ff[i],1.5f)),a1=(int)(46*env*powf(ff[i+1],1.5f));
            int v0=b.vert(pts[i].x+nx*w0,pts[i].y+ny*w0,alpha(c0,a0));
            int v1=b.vert(pts[i+1].x+nx*w1,pts[i+1].y+ny*w1,alpha(c1,a1));
            int v2=b.vert(pts[i+1].x-nx*w1,pts[i+1].y-ny*w1,alpha(c1,a1));
            int v3=b.vert(pts[i].x-nx*w0,pts[i].y-ny*w0,alpha(c0,a0));
            b.tri(v0,v1,v2); b.tri(v0,v2,v3);
        }
        for(int i=0;i<N;i++){
            float dx=pts[i+1].x-pts[i].x,dy=pts[i+1].y-pts[i].y;
            float l=sqrtf(dx*dx+dy*dy); if(l<1e-3f)continue;
            float nx=-dy/l,ny=dx/l;
            float w0=2.5f*powf(ff[i],.75f)+.35f;
            float w1=2.5f*powf(ff[i+1],.75f)+.35f;
            SDL_Color c0=mixc(C(255,255,255),C(255,206,132),1.f-ff[i]);
            SDL_Color c1=mixc(C(255,255,255),C(255,206,132),1.f-ff[i+1]);
            int a0=(int)(228*env*powf(ff[i],1.15f)),a1=(int)(228*env*powf(ff[i+1],1.15f));
            int v0=b.vert(pts[i].x+nx*w0,pts[i].y+ny*w0,alpha(c0,a0));
            int v1=b.vert(pts[i+1].x+nx*w1,pts[i+1].y+ny*w1,alpha(c1,a1));
            int v2=b.vert(pts[i+1].x-nx*w1,pts[i+1].y-ny*w1,alpha(c1,a1));
            int v3=b.vert(pts[i].x-nx*w0,pts[i].y-ny*w0,alpha(c0,a0));
            b.tri(v0,v1,v2); b.tri(v0,v2,v3);
        }
        Vec2 h=pts[0];
        b.gradCircle(h.x,h.y,12.f,alpha(C(255,236,196),(int)(120*env)),alpha(C(255,200,120),0),18);
        b.gradCircle(h.x,h.y,4.8f,alpha(C(255,255,250),(int)(235*env)),alpha(C(255,226,168),0),12);
        b.circle(h.x,h.y,1.9f,alpha(C(255,255,255),(int)(255*env)),8);
        float rr=t*.55f,c=cosf(rr),s=sinf(rr);
        float L=15.f*(.85f+.15f*sinf(t*2.1f)),Wd=1.5f,L2=8.5f;
        int ha=(int)(165*env);
        b.quad(h.x+c*L,h.y+s*L, h.x-s*Wd,h.y+c*Wd, h.x-c*L,h.y-s*L, h.x+s*Wd,h.y-c*Wd, alpha(C(255,250,235),ha));
        b.quad(h.x-s*L2,h.y+c*L2, h.x-c*Wd,h.y-s*Wd, h.x+s*L2,h.y-c*L2, h.x+c*Wd,h.y+s*Wd, alpha(C(255,250,235),(int)(ha*.8f)));
    }
    void update(float dt){
        idle+=dt; zoomIdle+=dt;
        if(idle<.05f)zoomIdle=0;
        if(zen)targetR=DEF_R;
        if(home)targetR=HOME_R;
        if(!locked){
            if(flying){
                float k=1-powf(.02f,dt);
                yaw=lerpf(yaw,flyYaw,k); pitch=lerpf(pitch,flyPitch,k);
                if(fabsf(yaw-flyYaw)<.002f&&fabsf(pitch-flyPitch)<.002f)flying=false;
            } else if(!dragging){
                yaw+=yawV*dt; pitch=clampf(pitch+pitchV*dt,-1.35f,1.35f);
                float d=powf(.12f,dt); yawV*=d; pitchV*=d;
                if(fabsf(yawV)<.004f)yawV=0;
                if(fabsf(pitchV)<.004f)pitchV=0;
            }
            if(zoomIdle>18&&!zen&&!home)targetR=lerpf(targetR,DEF_R,1-powf(.6f,dt));
            R=lerpf(R,targetR,1-powf(.008f,dt));
        } else yawV=pitchV=0;
        Vec2 ct=home?Vec2{HOME_CX,HOME_CY}:(zen?centerZen:centerHome);
        float ck=1-powf(.004f,dt);
        if(!locked){ center.x=lerpf(center.x,ct.x,ck); center.y=lerpf(center.y,ct.y,ck); }
        moving=dragging||flying||home||yawV!=0||pitchV!=0||fabsf(R-targetR)>=.4f
               ||fabsf(center.x-ct.x)>.4f||fabsf(center.y-ct.y)>.4f;
        selGlow=lerpf(selGlow,(selected>=0||selPt)?1.f:0.f,1-powf(.02f,dt));
        updRot(); updateSun(dt); updateLit(); starUpdate(dt);
        if(selPt){ Vec3 r=rotVec(selV); selZ=r.z; selP={center.x+R*r.x,center.y-R*r.y}; }
        if(projDirty||yaw!=cYaw||pitch!=cPitch||R!=cR||center.x!=cCx||center.y!=cCy){
            cYaw=yaw;cPitch=pitch;cR=R;cCx=center.x;cCy=center.y;projDirty=false;
            nearest=-1; nearestZ=0;
            for(size_t i=0;i<pv.size();i++){
                Vec3 r=rotVec(pv[i]); pz[i]=r.z;
                pp[i]={center.x+R*r.x,center.y-R*r.y};
                if(r.z>nearestZ){nearestZ=r.z;nearest=(int)i;}
            }
        }
    }
    void polyV(SDL_Renderer* r,const std::vector<Vec3>& v,SDL_Color col){
        SDL_Point pts[512]; int n=0;
        SDL_SetRenderDrawColor(r,col.r,col.g,col.b,col.a);
        for(size_t i=0;i<v.size();i++){
            Vec3 q=rotVec(v[i]);
            if(q.z>.02f){
                Vec2 s=proj(q);
                if(n<512)pts[n++]={(int)s.x,(int)s.y};
                else { if(n>1)SDL_RenderDrawLines(r,pts,n); pts[0]=pts[n-1]; n=1; }
            } else { if(n>1)SDL_RenderDrawLines(r,pts,n); n=0; }
        }
        if(n>1)SDL_RenderDrawLines(r,pts,n);
    }
    void drawDayNight(SDL_Renderer* r,Batch& b){
        Vec3 s=sunV;
        float len=sqrtf(s.x*s.x+s.y*s.y);
        const SDL_Color nightC=C(1,2,7), dayC=C(58,92,150);
        const int DA=196,LA=66;
        if(len<1e-4f){
            if(s.z<0)b.circle(center.x,center.y,R,alpha(nightC,DA),48);
            else     b.circle(center.x,center.y,R,alpha(dayC,LA),48);
            b.flush(r); return;
        }
        Vec3 u{-s.y/len,s.x/len,0};
        Vec3 v{s.y*u.z-s.z*u.y,s.z*u.x-s.x*u.z,s.x*u.y-s.y*u.x};
        float psi=atan2f(s.y,s.x);
        const int NS=44;
        const float TW=.34f,cT=cosf(TW),sT=sinf(TW);
        Vec2 A0[NS+1],AN[NS+1],AD[NS+1],BN[NS+1],BD[NS+1];
        for(int i=0;i<=NS;i++){
            float th=(float)M_PI*i/NS,ct=cosf(th),st=sinf(th);
            Vec3 t{u.x*ct+v.x*st,u.y*ct+v.y*st,u.z*ct+v.z*st};
            A0[i]=projClamp(t);
            AN[i]=projClamp({t.x*cT-s.x*sT,t.y*cT-s.y*sT,t.z*cT-s.z*sT});
            AD[i]=projClamp({t.x*cT+s.x*sT,t.y*cT+s.y*sT,t.z*cT+s.z*sT});
            float pn=psi+(float)M_PI/2+(float)M_PI*i/NS;
            BN[i]=proj({cosf(pn),sinf(pn),0});
            float pd=psi+(float)M_PI/2-(float)M_PI*i/NS;
            BD[i]=proj({cosf(pd),sinf(pd),0});
        }
        for(int i=0;i<NS;i++){
            int p0=b.vert(A0[i].x,A0[i].y,alpha(dayC,0));
            int p1=b.vert(AD[i].x,AD[i].y,alpha(dayC,LA));
            int p2=b.vert(AD[i+1].x,AD[i+1].y,alpha(dayC,LA));
            int p3=b.vert(A0[i+1].x,A0[i+1].y,alpha(dayC,0));
            b.tri(p0,p1,p2); b.tri(p0,p2,p3);
        }
        for(int i=0;i<NS;i++)
            b.quad(AD[i].x,AD[i].y,BD[i].x,BD[i].y,BD[i+1].x,BD[i+1].y,AD[i+1].x,AD[i+1].y,alpha(dayC,LA));
        for(int i=0;i<NS;i++){
            int p0=b.vert(A0[i].x,A0[i].y,alpha(nightC,0));
            int p1=b.vert(AN[i].x,AN[i].y,alpha(nightC,DA));
            int p2=b.vert(AN[i+1].x,AN[i+1].y,alpha(nightC,DA));
            int p3=b.vert(A0[i+1].x,A0[i+1].y,alpha(nightC,0));
            b.tri(p0,p1,p2); b.tri(p0,p2,p3);
        }
        for(int i=0;i<NS;i++)
            b.quad(AN[i].x,AN[i].y,BN[i].x,BN[i].y,BN[i+1].x,BN[i+1].y,AN[i+1].x,AN[i+1].y,alpha(nightC,DA));
        b.flush(r);
    }
    void drawSphere(SDL_Renderer* r,Batch& b,float warm){
        SDL_SetRenderDrawBlendMode(r,SDL_BLENDMODE_NONE);
        b.gradCircle(center.x,center.y,rimR(),mixc(Pal::sphereIn,C(30,26,22),warm*.5f),Pal::sphereOut,R>520.f?128:64);
        b.flush(r);
        SDL_SetRenderDrawBlendMode(r,SDL_BLENDMODE_BLEND);
    }
    static float litPos(float dl){ return clampf((dl+.42f)/.80f,0.f,1.f); }

    static SDL_Color litRamp(float t){
        struct S{ float t; SDL_Color c; };
        static const S s[6]={
            {0.00f,C(255,238,196)},
            {0.22f,C(255,198,124)},
            {0.45f,C(214,182,164)},
            {0.60f,C(168,166,196)},
            {0.78f,C( 96,142,222)},
            {1.00f,C( 46,128,255)}
        };
        t=clampf(t,0.f,1.f);
        int i=0; while(i<4&&t>s[i+1].t)++i;
        return mixc(s[i].c,s[i+1].c,(t-s[i].t)/(s[i+1].t-s[i].t));
    }
    void drawWire(SDL_Renderer* r,Batch& b,int W,int H,float warm){
        if(sunV.z>-.35f){
            b.gradCircle(center.x+sunV.x*R*.22f,center.y-sunV.y*R*.22f,R*.7f,
                         alpha(mixc(C(96,128,190),C(190,140,80),warm),20),alpha(C(96,128,190),0),28);
            b.flush(r);
        }
        drawDayNight(r,b);
        for(auto& g:gridv)polyV(r,g,alpha(Pal::wire,26));
        for(auto& l:landv)polyV(r,l,alpha(Pal::wire,90));
    int shift=2,stride=W>>shift,rows=H>>shift;
    size_t need=(size_t)stride*rows;
    if(cells.size()<need)cells.resize(need);
    memset(cells.data(),0,need);
    for(int k=0;k<LITN;k++)core[k].clear();
    for(int k=0;k<HALON;k++){halo1[k].clear();halo2[k].clear();}
    float zs=clampf(sqrtf(R/DEF_R),.8f,2.f);
    if(places&&dotFade>.01f){
        for(int idx:order){
            if(idx<0||(size_t)idx>=pz.size())continue;
            if(pz[idx]<=.03f)continue;
            Vec2 s=pp[idx];
            if(s.x<0||s.y<0||s.x>=W||s.y>=H)continue;
            size_t ci=(size_t)((int)s.y>>shift)*stride+((int)s.x>>shift);
            if(ci>=need||cells[ci])continue;
            cells[ci]=1;
            float rr=dotR[idx]*zs; if(rr<.8f)rr=.8f;
            int d=(int)(rr*2+.5f); if(d<1)d=1;

            float dl=((size_t)idx<lit.size())?lit[idx]:1.f;
            float lp=litPos(dl);
            int k=(int)(lp*(LITN-1)+.5f);
            if(k<0)k=0; if(k>LITN-1)k=LITN-1;
            core[k].push_back({(int)(s.x-rr),(int)(s.y-rr),d,d});

            float ng=clampf((.45f-lp)/.45f,0.f,1.f);
            ng=ng*ng*(3.f-2.f*ng);
            if(ng>.02f){
                int hk=(int)(ng*(HALON-1)+.5f);
                if(hk<1)hk=1; if(hk>HALON-1)hk=HALON-1;
                int h1=(int)(d*2.0f+.5f); if(h1<3)h1=3;
                int h2=(int)(d*3.4f+.5f); if(h2<5)h2=5;
                halo1[hk].push_back({(int)s.x-h1/2,(int)s.y-h1/2,h1,h1});
                halo2[hk].push_back({(int)s.x-h2/2,(int)s.y-h2/2,h2,h2});
            }
        }
    }
    for(int k=1;k<HALON;k++){
        if(halo2[k].empty())continue;
        float a=(float)k/(float)(HALON-1);
        SDL_SetRenderDrawColor(r,255,186,110,(Uint8)((20.f*a+.5f)*dotFade));
        SDL_RenderFillRects(r,halo2[k].data(),(int)halo2[k].size());
    }
    for(int k=1;k<HALON;k++){
        if(halo1[k].empty())continue;
        float a=(float)k/(float)(HALON-1);
        SDL_SetRenderDrawColor(r,255,214,150,(Uint8)((46.f*a+.5f)*dotFade));
        SDL_RenderFillRects(r,halo1[k].data(),(int)halo1[k].size());
    }
    for(int k=0;k<LITN;k++){
        if(core[k].empty())continue;
        SDL_Color c=litRamp((float)k/(float)(LITN-1));
        SDL_SetRenderDrawColor(r,c.r,c.g,c.b,(Uint8)(232.f*dotFade));
        SDL_RenderFillRects(r,core[k].data(),(int)core[k].size());
    }
    b.flush(r);
    }
    void toTarget(SDL_Renderer* r,Batch& b,SDL_Texture* t){
        b.flush(r);
        SDL_RenderSetClipRect(r,nullptr);
        SDL_SetRenderTarget(r,t);
        SDL_SetRenderDrawBlendMode(r,SDL_BLENDMODE_NONE);
        SDL_SetRenderDrawColor(r,0,0,0,0); SDL_RenderClear(r);
        SDL_SetRenderDrawBlendMode(r,SDL_BLENDMODE_BLEND);
    }
    void fromTarget(SDL_Renderer* r,Batch& b){
        b.flush(r);
        SDL_SetRenderTarget(r,nullptr);
        SDL_SetRenderDrawBlendMode(r,SDL_BLENDMODE_BLEND);
    }
    void draw(SDL_Renderer* r,Batch& b,float t,int W,int H,float warm){
        scrW=W; scrH=H;
        SDL_Rect box=bbox(W,H);
        if(!moving&&cache){
            bool same=cacheValid&&yaw==kYaw&&pitch==kPitch&&R==kR&&warm==kWarm
                      &&center.x==kCx&&center.y==kCy
                      &&sunV.x==kSun.x&&sunV.y==kSun.y&&sunV.z==kSun.z&&rev==kRev;
            if(!same){
                toTarget(r,b,cache);
                drawSphere(r,b,warm); drawWire(r,b,W,H,warm);
                fromTarget(r,b);
                cacheValid=true;kYaw=yaw;kPitch=pitch;kR=R;kWarm=warm;kSun=sunV;kRev=rev;
                kCx=center.x;kCy=center.y;
            }
            if(box.w>0)SDL_RenderCopy(r,cache,&box,&box);
        } else {
            cacheValid=false;
            if(sph){
                if(!sphValid||R!=sR||warm!=sWarm){
                    toTarget(r,b,sph); drawSphere(r,b,warm); fromTarget(r,b);
                    sphValid=true;sR=R;sWarm=warm;
                }
                if(box.w>0)SDL_RenderCopy(r,sph,&box,&box);
            } else drawSphere(r,b,warm);
            drawWire(r,b,W,H,warm);
        }
        float rf=rimR();
        int rs=R>520.f?150:52;
        for(int k=0;k<3;k++)b.ring(center.x,center.y,rf*(1.f+.013f*k),2.6f,alpha(Pal::wire,26-k*8),rs);
        b.ring(center.x,center.y,rf,1.2f,alpha(Pal::wire,72),rs);
        if(selGlow>.01f){
            float br=.5f+.5f*sinf(t*.55f);
            float a=selGlow*(.72f+.28f*br);
            SDL_Color gc=mixc(C(255,255,255),Pal::liveSoft,.22f);
            b.glowRing(center.x,center.y,rf*.998f,rf*1.03f,alpha(gc,(int)(115*a)),alpha(gc,(int)(42*a)),72);
            b.glowRing(center.x,center.y,rf*1.03f,rf*1.11f,alpha(gc,(int)(42*a)),alpha(gc,0),72);
            b.glowRing(center.x,center.y,rf*1.11f,rf*1.26f,alpha(gc,(int)(14*a)),alpha(gc,0),56);
        }
        if(!zen&&!home){
            float breath=.5f+.5f*sinf(t*.9f);
            b.ring(center.x,center.y,26+breath*1.6f,1.4f,alpha(Pal::wire,(int)(96+26*breath)),24);
            for(int k=0;k<4;k++){ float a=k*(float)M_PI/2;
                b.line(center.x+cosf(a)*32,center.y+sinf(a)*32,center.x+cosf(a)*40,center.y+sinf(a)*40,1.4f,alpha(Pal::wire,110)); }
        }
        Vec2 sp{0,0}; bool haveSp=false;
        if(selPt&&selZ>.02f){ sp=selP; haveSp=true; }
        else if(selected>=0&&(size_t)selected<pp.size()&&pz[selected]>.02f){ sp=pp[selected]; haveSp=true; }
        if(haveSp){
            Vec2 s=sp;
            float p=.5f+.5f*sinf(t*.7f);
            float p2=.5f+.5f*sinf(t*.7f-1.05f);
            b.gradCircle(s.x,s.y,26+10*p,alpha(Pal::live,(int)(46+26*p)),alpha(Pal::live,0),24);
            b.ring(s.x,s.y,19+8*p2,1.3f,alpha(Pal::live,(int)(34+46*(1.f-p2))),28);
            b.ring(s.x,s.y,11+3.5f*p,1.9f,alpha(Pal::liveSoft,(int)(130+90*p)),24);
            b.circle(s.x,s.y,3.4f+.7f*p,Pal::liveSoft,12);
            b.circle(s.x,s.y,1.6f,C(255,255,255),8);
        }
        if(!zen&&!home&&nearest>=0&&nearest!=selected&&(size_t)nearest<pp.size()&&nearestZ>.5f
           &&!(haveSp&&fabsf(pp[nearest].x-sp.x)+fabsf(pp[nearest].y-sp.y)<14.f)){
            Vec2 s=pp[nearest];
            float p=.5f+.5f*sinf(t*1.1f);
            b.ring(s.x,s.y,9+p*2,1.3f,alpha(Pal::wire,(int)(120+40*p)),18);
            b.circle(s.x,s.y,2.2f,alpha(Pal::text,220),8);
        }
        starDraw(r,b,t);
        b.flush(r);
    }
};

static const int FONT_PX[6]={15,17,20,24,30,42};
enum FontId { F15,F17,F20,F24,F30,F42 };
static const char* FONT_FILES[6]={"font.ttf","font_cjk.ttf","font_ar.ttf","font_he.ttf","font_th.ttf","font_hi.ttf"};
struct Fonts {
    TTF_Font* f[6][6]={}; std::string paths[6];
    void init(){ for(int i=0;i<6;i++)paths[i]=asset(FONT_FILES[i]); }
    TTF_Font* get(int s,int k){
        if(s<0||s>5||k<0||k>5)return nullptr;
        if(f[s][k])return f[s][k];
        if(paths[k].empty())return nullptr;
        f[s][k]=TTF_OpenFont(paths[k].c_str(),FONT_PX[s]);
        return f[s][k];
    }
    int famFor(unsigned cp,int size){
        if(cp<0x0590||cp>0xFFFF)return 0;
        int k=0;
        if(cp>=0x0590&&cp<=0x05FF)k=3;
        else if((cp>=0x0600&&cp<=0x06FF)||(cp>=0x0750&&cp<=0x077F)||(cp>=0xFB50&&cp<=0xFEFF))k=2;
        else if(cp>=0x0900&&cp<=0x097F)k=5;
        else if(cp>=0x0E00&&cp<=0x0E7F)k=4;
        else if((cp>=0x1100&&cp<=0x11FF)||(cp>=0x2E80&&cp<=0x9FFF)||(cp>=0xA960&&cp<=0xA97F)||
                (cp>=0xAC00&&cp<=0xD7AF)||(cp>=0xF900&&cp<=0xFAFF)||(cp>=0xFF00&&cp<=0xFFEF))k=1;
        if(!k)return 0;
        TTF_Font* ft=get(size,k);
        if(!ft)return 0;
        if(TTF_GlyphIsProvided(ft,(Uint16)cp))return k;
        TTF_Font* base=get(size,0);
        if(base&&TTF_GlyphIsProvided(base,(Uint16)cp))return 0;
        return k;
    }
    void close(){ for(int i=0;i<6;i++)for(int k=0;k<6;k++)if(f[i][k]){TTF_CloseFont(f[i][k]);f[i][k]=nullptr;} }
};
struct Run { int fam; std::string text; };
static unsigned utf8Next(const std::string& s,size_t& i){
    unsigned char c=(unsigned char)s[i]; unsigned cp; int ex;
    if(c<0x80){cp=c;ex=0;}
    else if((c&0xE0)==0xC0){cp=c&0x1F;ex=1;}
    else if((c&0xF0)==0xE0){cp=c&0x0F;ex=2;}
    else if((c&0xF8)==0xF0){cp=c&0x07;ex=3;}
    else {cp=c;ex=0;}
    i++;
    for(int k=0;k<ex&&i<s.size();k++){ unsigned char d=(unsigned char)s[i];
        if((d&0xC0)!=0x80)break; cp=(cp<<6)|(d&0x3F); i++; }
    return cp;
}

struct Worker {
    std::mutex m; std::condition_variable cv;
    std::deque<std::function<void()>> q;
    std::thread th; bool run=true;
    void start(){ th=std::thread([this]{ loop(); }); }
    void loop(){
        for(;;){
            std::function<void()> f;
            { std::unique_lock<std::mutex> lk(m);
                cv.wait(lk,[this]{ return !run||!q.empty(); });
                if(!run)return;
                f=std::move(q.front()); q.pop_front(); }
            if(!gQuit.load()){ try{ f(); }catch(...){ logLine("worker exception"); } }
        }
    }
    void post(std::function<void()> f,bool front=false){
        { std::lock_guard<std::mutex> lk(m); if(!run)return;
            if(front)q.push_front(std::move(f)); else q.push_back(std::move(f)); }
        cv.notify_one();
    }
    void stop(){ { std::lock_guard<std::mutex> lk(m); run=false; q.clear(); } cv.notify_all(); if(th.joinable())th.join(); }
    ~Worker(){ stop(); }
};
template<class T> struct Slot {
    std::mutex m; std::atomic<bool> ready{false},busy{false}; T val{};
    bool take(T& o){ if(!ready.exchange(false))return false; std::lock_guard<std::mutex> lk(m); o=std::move(val); return true; }
    void put(T v){ { std::lock_guard<std::mutex> lk(m); val=std::move(v); } ready=true; busy=false; }
};
struct Toast { std::string text; float age=0; SDL_Color col; };
struct UiHit { int id; SDL_Rect r; int data; };
enum { H_NONE=0,H_PLAYPAUSE,H_FAVNOW,H_BACK,H_ROW,H_FIELD,H_RETRY,
H_PLACE,H_TAB,H_HOME,H_CARD,H_PC_CLOSE,H_PC_RETRY,H_GTA_ROW,H_GTA_GAME };
enum Screen { SCR_GLOBE,SCR_CITY,SCR_SEARCH,SCR_LIB };
enum LibTab { LT_FAV=0,LT_HIST=1,LT_PRESET=2,LT_SOUND=3 };
static const int W=1280,H=720,PW=430,NB=32;
static std::string swkbdInput(const std::string& init,const char* guide){
    SwkbdConfig c;
    if(R_FAILED(swkbdCreate(&c,0)))return init;
    swkbdConfigMakePresetDefault(&c);
    swkbdConfigSetGuideText(&c,guide);
    swkbdConfigSetStringLenMax(&c,64);
    swkbdConfigSetInitialText(&c,init.c_str());
    char out[65]={0};
    Result rc=swkbdShow(&c,out,sizeof out);
    swkbdClose(&c);
    return R_SUCCEEDED(rc)?std::string(out):init;
}
static const char* wxCond(int c){
    if(c==0)return "Clear";
    if(c<=2)return "Partly cloudy";
    if(c==3)return "Cloudy";
    if(c==45||c==48)return "Fog";
    if(c>=51&&c<=57)return "Drizzle";
    if((c>=61&&c<=67)||(c>=80&&c<=82))return "Rain";
    if((c>=71&&c<=77)||c==85||c==86)return "Snow";
    if(c>=95)return "Thunder";
    return "—";
}
static const char* GTA_AUDIO="https://audio.gtaradio.net";
static const char* GTA_STATUS="https://audio.gtaradio.net/status-json.xsl";

struct GtaStation{ std::string mount,game,name,genre,title,url; int listeners=0,bitrate=0; };
struct GtaRes{ bool ok=false; std::string err; std::vector<GtaStation> v; };
struct GtaGameDef{ const char* key; const char* label; float hue; };
static const GtaGameDef GTA_GAMES[]={
{"6","GTA VI",318.f},
{"5","GTA V",200.f},{"4","GTA IV",38.f},{"sa","San Andreas",95.f},{"vc","Vice City",322.f},
{"vcs","Vice City Stories",286.f},{"lcs","Liberty City Stories",176.f},{"3","GTA III",210.f}
};
static const int GTA_NG=8;
struct GtaGradDef{ float h[4],s[4],v[4]; };
static const GtaGradDef GTA_GRAD[GTA_NG]={
{{318,286,196,335},{.72f,.66f,.58f,.68f},{1.f,.95f,.95f,1.f}},   // VI
{{205,186,224,168},{.75f,.70f,.55f,.45f},{1.f,1.f,.95f,.90f}},   // V
{{ 36, 22, 48,210},{.80f,.75f,.65f,.35f},{1.f,.95f,.90f,.80f}},  // IV
{{100, 48, 26,140},{.70f,.80f,.75f,.50f},{.95f,1.f,.95f,.85f}},  // SA
{{322,190,278,335},{.80f,.75f,.65f,.70f},{1.f,1.f,.95f,1.f}},    // VC
{{286,320,214,262},{.70f,.75f,.60f,.55f},{1.f,1.f,.95f,.90f}},   // VCS
{{176,208,192,230},{.65f,.60f,.55f,.45f},{.95f,.90f,.90f,.85f}}, // LCS
{{212,236,198,250},{.65f,.55f,.50f,.40f},{.90f,.85f,.90f,.80f}}  // III
};
static std::string gtaNormGame(std::string g){ return lowerStr(g); }
static int gtaGameIdx(const std::string& g){
    std::string k=gtaNormGame(g);
    for(int i=0;i<GTA_NG;i++)if(k==GTA_GAMES[i].key)return i;
    return GTA_NG;
}
static const char* gtaGameLabel(const std::string& g){ int i=gtaGameIdx(g); return i<GTA_NG?GTA_GAMES[i].label:"GTA"; }
static float gtaHue(const std::string& g){ int i=gtaGameIdx(g); return i<GTA_NG?GTA_GAMES[i].hue:210.f; }
static std::string gtaPath(const std::string& rel){
    static std::map<std::string,std::string> cache;
    auto it=cache.find(rel);
    if(it!=cache.end())return it->second;
    std::string found;
    for(const std::string& path:{gAppDir+"gta/"+rel,gAppDir+"assets/gta/"+rel,gCacheDir+"gta/"+rel,std::string("romfs:/gta/")+rel}){
        if(fileExists(path)){found=path;break;}
    }
    cache.emplace(rel,found);
    return found;
}
static const char* GTA_NAMES=
    "3/head=Head Radio|Pop · rock;3/class=Double Cleff FM|Classical;3/double=Double Cleff FM|Classical;3/kjah=K-JAH|Reggae · dub;"
    "3/rise=Rise FM|House · trance;3/lips=Lips 106|Pop · funk;3/msx=MSX FM|Drum & bass;3/game=Game Radio|East coast rap;"
    "3/flash=Flashback 95.6|80s;3/chat=Chatterbox FM|Talk;"
    "4/afro_beat=International Funk 99|Afrobeat · funk;4/babylon=Tuff Gong Radio|Reggae · dub;"
    "4/beat_95=The Beat 102.7|Hip-hop · R&B;4/bobby_konders=Massive B Soundsystem 96.9|Dancehall;"
    "4/classical_ambient=The Journey|Ambient · new age;4/dance_mix=Electro Choc|Electro house;"
    "4/dance_rock=Radio Broker|Dance rock;4/fusion_fm=Fusion FM|Jazz fusion;4/hardcore=Liberty City Hardcore|Hardcore punk;"
    "4/jazz_nation=Jazz Nation Radio 108.5|Jazz;4/k109_the_studio=K109 The Studio|Disco;4/lazlow=Integrity 2.0|Talk;"
    "4/liberty_rock=Liberty Rock Radio 97.8|Classic rock;4/meditation=Self Actualization FM|Ambient;"
    "4/ny_classics=The Classics 104.1|Oldschool hip-hop;4/plr=Public Liberty Radio|Talk;4/ramjamfm=Ram Jam FM|Reggae · dub;"
    "4/san_juan_sounds=San Juan Sounds|Reggaeton · bachata;4/the_vibe=The Vibe 98.8|R&B · soul;4/vcfm=Vice City FM|80s;"
    "4/vladivostok=Vladivostok FM|Russian pop · rock;4/wktt=We Know The Truth|Talk;"
    "sa/playback-fm=Playback FM|Oldschool hip-hop;sa/k-rose=K-Rose|Country;sa/k-dst=K-DST|Classic rock;"
    "sa/bounce-fm=Bounce FM|Funk · disco;sa/sfur=SF-UR|House;sa/radio-los-santos=Radio Los Santos|West coast rap;"
    "sa/radio-x=Radio X|Alternative · grunge;sa/csr=CSR 103.9|New jack swing;sa/k-jah=K-JAH West|Reggae;"
    "sa/master-sounds=Master Sounds 98.3|Funk · rare groove;sa/wctr=WCTR|Talk;"
    "vc/wild=Wildstyle Pirate Radio|Hip-hop · electro;vc/fever=Fever 105|Disco · funk;vc/vrock=V-Rock|Rock · metal;"
    "vc/vcpr=VCPR|Talk;vc/espant=Radio Espantoso|Latin · salsa;vc/emotion=Emotion 98.3|Ballads;"
    "vc/wave=Wave 103|New wave · synthpop;vc/flash=Flash FM|80s pop;vc/kchat=K-Chat|Talk;"
    "vcs/vrock=V-Rock|Rock · metal;vcs/flash=Flash FM|80s pop;vcs/paradise=Paradise FM|Disco · funk;"
    "vcs/vcfl=Vice City For Lovers|Latin · soul;vcs/vcpr=VCPR|Talk;vcs/wave=Wave 103|New wave;"
    "vcs/emotion=Emotion 98.3|Ballads;vcs/espant=Radio Espantoso|Latin;vcs/fresh=Fresh 92.7|Hip-hop · R&B;"
    "lcs/head=Head Radio|Pop · rock;lcs/double=Double Cleff FM|Classical;lcs/kjah=K-JAH|Reggae;"
    "lcs/rise=Rise FM|House;lcs/lips=Lips 106|Pop · funk;lcs/msx=MSX FM|Drum & bass;"
    "lcs/lcj=Liberty City Jam|Hip-hop;lcs/lcfr=Liberty City Free Radio|Talk;lcs/flash=Flashback FM|80s;lcs/mundo=Radio Mundo|World · latin;";
static void gtaLabel(const std::string& mount,std::string& name,std::string& genre){
    static std::map<std::string,std::pair<std::string,std::string>> m;
    if(m.empty()){
        std::string s=GTA_NAMES; size_t i=0;
        while(i<s.size()){
            size_t j=s.find(';',i); if(j==std::string::npos)j=s.size();
            std::string e=s.substr(i,j-i); i=j+1;
            size_t eq=e.find('='),pp=e.find('|'); if(eq==std::string::npos)continue;
            m[e.substr(0,eq)]={e.substr(eq+1,pp==std::string::npos?std::string::npos:pp-eq-1),pp==std::string::npos?"":e.substr(pp+1)};
        }
    }
    auto it=m.find(lowerStr(mount));
    if(it!=m.end()){name=it->second.first;genre=it->second.second;return;}
    size_t sl=mount.find('/'); std::string t=sl==std::string::npos?mount:mount.substr(sl+1);
    for(auto& c:t)if(c=='-'||c=='_')c=' ';
    if(!t.empty())t[0]=(char)toupper((unsigned char)t[0]);
    name=t.empty()?mount:t; genre="Video game music";
}
static std::string gtaStationAsset(const char* folder,const std::string& mount){
    std::string nested=std::string(folder)+mount+".png";
    if(!gtaPath(nested).empty())return nested;
    size_t slash=mount.find('/');
    if(slash!=std::string::npos){
        std::string flat=std::string(folder)+mount.substr(0,slash)+"_"+mount.substr(slash+1)+".png";
        if(!gtaPath(flat).empty())return flat;
        std::string shortName=std::string(folder)+mount.substr(slash+1)+".png";
        if(!gtaPath(shortName).empty())return shortName;
    }
    return nested;
}
static std::string gtaLogoRel(const std::string& mount){return gtaStationAsset("stations/",mount);}
static bool gtaLoadCatalog(std::map<std::string,std::vector<GtaStation>>& out){
    std::string p=gtaPath("catalog.json"),body;
    if(p.empty()||!readFile(p,body))return false;
    J j(body); if(!j.eat('{'))return false;
    std::string k;
    while(objNext(j,k)){
        if(k!="games"){j.skip();continue;}
        if(!j.eat('['))break;
        while(arrNext(j)){
            if(!j.eat('{'))break;
            std::string gk,key; std::vector<GtaStation> sts;
            while(objNext(j,gk)){
                if(gk=="key")key=gtaNormGame(trimStr(j.str()));
                else if(gk=="stations"){
                    if(!j.eat('['))break;
                    while(arrNext(j)){
                        if(!j.eat('{'))break;
                        GtaStation st; std::string sk;
                        while(objNext(j,sk)){
                            if(sk=="mount")st.mount=trimStr(j.str());
                            else if(sk=="url")st.url=trimStr(j.str());
                            else j.skip();
                        }
                        j.eat('}');
                        if(!st.mount.empty())sts.push_back(std::move(st));
                        if(!j.ok)break;
                    }
                    j.eat(']');
                } else j.skip();
            }
            j.eat('}');
            if(!key.empty()&&!sts.empty()){
                for(auto& st:sts){
                    st.game=key;
                    if(st.mount.find('/')==std::string::npos)st.mount=key+"/"+st.mount;
                    if(st.url.empty())st.url=std::string(GTA_AUDIO)+"/"+st.mount;
                    gtaLabel(st.mount,st.name,st.genre);
                }
                std::sort(sts.begin(),sts.end(),[](const GtaStation&a,const GtaStation&b){return a.name<b.name;});
                out[key]=std::move(sts);
            }
            if(!j.ok)break;
        }
        j.eat(']');
    }
    return !out.empty();
}
static void gtaOne(J& j,GtaStation& st){
    if(!j.eat('{'))return;
    std::string k,lu,sn; bool audio=true;
    while(objNext(j,k)){
        if(k=="listenurl")lu=j.str();
        else if(k=="server_name")sn=j.str();
        else if(k=="title")st.title=trimStr(j.str());
        else if(k=="listeners")st.listeners=(int)j.num();
        else if(k=="bitrate")st.bitrate=(int)j.num();
        else if(k=="server_type"){std::string v=lowerStr(j.str());audio=v.rfind("audio",0)==0;}
        else j.skip();
    }
    j.eat('}');
    std::string path; size_t p=lu.find("://");
    if(p!=std::string::npos){size_t q=lu.find('/',p+3);if(q!=std::string::npos)path=lu.substr(q);}
    if(path.empty()&&!sn.empty()&&sn[0]=='/')path=sn;
    if(!path.empty()&&path[0]=='/')path.erase(0,1);
    if(!audio||path.find('/')==std::string::npos)return;
    st.mount=path; st.game=gtaNormGame(path.substr(0,path.find('/')));
    st.url=std::string(GTA_AUDIO)+"/"+path;
    gtaLabel(path,st.name,st.genre);
}
static GtaRes parseGtaStatus(const std::string& body){
    GtaRes r; J j(body); if(!j.eat('{'))return r;
    std::string k;
    while(objNext(j,k)){
        if(k!="icestats"){j.skip();continue;}
        if(!j.eat('{'))break;
        std::string ik;
        while(objNext(j,ik)){
            if(ik!="source"){j.skip();continue;}
            if(j.at('[')){
                if(!j.eat('['))break;
                while(arrNext(j)){GtaStation st;gtaOne(j,st);if(!st.mount.empty())r.v.push_back(std::move(st));if(!j.ok)break;}
                j.eat(']');
            } else {GtaStation st;gtaOne(j,st);if(!st.mount.empty())r.v.push_back(std::move(st));}
        }
        j.eat('}');
    }
    r.ok=!r.v.empty(); return r;
}
static GtaRes fetchGtaLive(){
    std::string path=gCacheDir+"gta_live.json",body,err;
    body=httpGet(GTA_STATUS,err,10,false,true,UA_APP);
    if(!body.empty()){
        GtaRes r=parseGtaStatus(body);
        if(r.ok){writeFile(path,body);return r;}
    }
    if(readFile(path,body)){GtaRes r=parseGtaStatus(body);if(r.ok)return r;}
    GtaRes r; r.err=err.empty()?"catalog unavailable":err; return r;
}
static uint32_t pcHash(const std::string& s);

struct GtaVDef{ const char* slug; const char* name; const char* genre; };
static const GtaVDef GTA_V[]={
{"los-santos-rock-radio","Los Santos Rock Radio","Classic rock"},
{"non-stop-pop-fm","Non-Stop-Pop FM","Pop"},
{"radio-los-santos","Radio Los Santos","Hip-hop"},
{"channel-x","Channel X","Punk rock"},
{"west-coast-classics","West Coast Classics","West coast hip-hop"},
{"rebel-radio","Rebel Radio","Country"},
{"los-santos-underground-radio","Los Santos Underground Radio","Techno · house"},
{"soulwax-fm","Soulwax FM","Electro · techno"},
{"flylo-fm","FlyLo FM","Experimental · IDM"},
{"blaine-county-radio","Blaine County Radio","Talk"},
{"blue-ark","Blue Ark","Reggae · dub"},
{"east-los-fm","East Los FM","Latin · electro"},
{"the-lowdown","The Lowdown 91.1","Soul · R&B"},
{"worldwide-fm","WorldWide FM","Jazz · electronic"},
{"space-103-2","Space 103.2","Funk · disco"},
{"vinewood-boulevard-radio","Vinewood Boulevard Radio","Garage rock"},
{"radio-mirror-park","Radio Mirror Park","Indie · synthpop"},
{"the-lab","The Lab","Hip-hop"},
{"blonded-los-santos","blonded Los Santos 97.8 FM","R&B · hip-hop"},
{"ifruit-radio","iFruit Radio","Hip-hop · grime"},
{"kult-fm","KULT FM 99.1","Post-punk · new wave"},
{"still-slipping-los-santos","Still Slipping Los Santos","UK garage · bass"},
{"motomami-los-santos","MOTOMAMI Los Santos","Latin · experimental"},
{"music-locker-radio","Music Locker Radio","House · disco"},
{"west-coast-talk-radio","West Coast Talk Radio","Talk"}
};

static const char* IA_PM="http://ia601008.us.archive.org/31/items/Radio-files-pause-menu/";
static const char* IA_MK="http://ia600909.us.archive.org/17/items/radio-for-my-website-mako/";
static const char* IA_SG="http://ia600107.us.archive.org/34/items/gta-5-radio-songs_202603/";

struct GtaVSrc{ const char* slug; const char* item; const char* file;
                const char* item2; const char* file2; int seconds; };
static const GtaVSrc GTA_V_SRC[]={
{"los-santos-rock-radio",IA_PM,"GTA V & GTA Online — Los Santos Rock Radio ｜ Full radio station.mp3",IA_MK,"Gta 5 - Los Santos Rock Radio (All Songs).mp3",17241},
{"non-stop-pop-fm",IA_PM,"GTA V & GTA Online — Non-Stop-Pop FM ｜ Full radio station.mp3",IA_MK,"Gta 5 NON-STOP POP radio (all songs).mp3",13878},
{"radio-los-santos",IA_PM,"GTA V & GTA Online — Radio Los Santos ｜ Full radio station.mp3",IA_MK,"Radio Los Santos [GTA V].mp3",21384},
{"channel-x",IA_PM,"GTA V & GTA Online — Channel X ｜ Full radio station.mp3",IA_MK,"GTA V - Channel X Radio Station (Full Radio Station).mp3",5509},
{"west-coast-classics",IA_PM,"GTA V & GTA Online — West Coast Classics ｜ Full radio station.mp3",IA_MK,"Radio West Coast Classics _ GTA V.mp3",15512},
{"rebel-radio",IA_PM,"GTA V & GTA Online — Rebel Radio ｜ Full radio station.mp3",IA_MK,"Rebel Radio [Grand Theft Auto V].mp3",4958},
{"los-santos-underground-radio",IA_PM,"GTA V & GTA Online — Los Santos Underground Radio ｜ Full radio station.mp3",nullptr,nullptr,16734},
{"soulwax-fm",IA_PM,"GTA V & GTA Online — Soulwax FM ｜ Full radio station.mp3",IA_MK,"Soulwax FM [GTA V].mp3",3259},
{"flylo-fm",IA_PM,"GTA V & GTA Online — FlyLo FM ｜ Full radio station.mp3",IA_MK,"FlyLo FM [GTA V] ALL SONGS__ [JglBzas56qY].mp3",7769},
{"blaine-county-radio",IA_MK,"Blaine County Talk Radio [GTA V] [aaXui87cF5Y].mp3",IA_SG,"Blaine County Talk Radio [GTA V] [aaXui87cF5Y].mp3",5079},
{"blue-ark",IA_PM,"GTA V & GTA Online — Blue Ark ｜ Full radio station.mp3",IA_MK,"Blue Ark [GTA V].mp3",8134},
{"east-los-fm",IA_PM,"GTA V & GTA Online — East Los FM ｜ Full radio station.mp3",IA_MK,"EAST LOS FM gta 5 - radio completa.mp3",2702},
{"the-lowdown",IA_PM,"GTA V & GTA Online — The Lowdown 91.1 ｜ Full radio station.mp3",IA_MK,"The Lowdown 91.1 [Grand Theft Auto V].mp3",8539},
{"worldwide-fm",IA_PM,"GTA V & GTA Online — Worldwide FM ｜ Full radio station.mp3",IA_MK,"WorldWide FM [GTA V].mp3",18462},
{"space-103-2",IA_PM,"GTA V & GTA Online — Space 103.2 ｜ Full radio station.mp3",IA_MK,"Space 103.2 [Grand Theft Auto V].mp3",11298},
{"vinewood-boulevard-radio",IA_PM,"GTA V & GTA Online — Vinewood Boulevard Radio ｜ Full radio station.mp3",IA_MK,"Vinewood Boulevard Radio [GTA V] [8dCwPmAPcLw].mp3",5536},
{"radio-mirror-park",IA_PM,"GTA V & GTA Online — Radio Mirror Park ｜ Full radio station.mp3",IA_MK,"Radio Mirror Park [Grand Theft Auto V].mp3",12773},
{"the-lab",IA_PM,"GTA V & GTA Online — The Lab ｜ Full radio station.mp3",IA_MK,"The Lab [GTA V] [4J6JK7ich6E].mp3",3456},
{"blonded-los-santos",IA_PM,"GTA V & GTA Online — blonded Los Santos 97.8 FM ｜ Full radio station.mp3",IA_MK,"blonded Los Santos 97.8 FM [GTA V] [XNxUKtODPDs].mp3",6139},
{"ifruit-radio",IA_PM,"GTA V & GTA Online — iFruit Radio ｜ Full radio station.mp3",IA_MK,"GTA V & GTA Online — iFruit Radio _ Full radio station [fpvJaphZ2_g].mp3",5201},
{"kult-fm",IA_PM,"GTA Online — Kult FM ｜ Full radio station.mp3",IA_MK,"K.U.L.T. 99.1 FM [GTA Online_ The Cayo Perico Update] ALL SONGS__ [FY9EiOllRhE].mp3",8411},
{"still-slipping-los-santos",IA_PM,"GTA Online — Still Slipping Los Santos ｜ Full radio station.mp3",IA_MK,"Still Slipping Los Santos (Hosted by DJ Joy Orbison) [GTA V] W_ TIMESTAMPS- Cayo Perico Heist [FwbNs4oMmF8].mp3",4364},
{"motomami-los-santos",IA_PM,"GTA Online — MOTOMAMI Los Santos ｜ Full radio station.mp3",IA_MK,"MOTOMAMI Los Santos_ Radio [xNjHCRa-pFk].mp3",11759},
{"music-locker-radio",IA_PM,"GTA Online — Music Locker Radio ｜ Full radio station.mp3",nullptr,nullptr,27245},
{"west-coast-talk-radio",IA_MK,"WCTR (West Coast Talk Radio) (GTA V) [IhCFJnaYvnI].mp3",IA_SG,"WCTR (West Coast Talk Radio) (GTA V) [IhCFJnaYvnI].mp3",5815}
};

static const GtaVSrc* gtaVSrc(const std::string& slug){
    for(auto& s:GTA_V_SRC)if(slug==s.slug)return &s;
    return nullptr;
}

static const char* IA_VI="http://ia600809.us.archive.org/6/items/gta-6-radio-audio-bk-20261124-v2/";

static const GtaVDef GTA_VI[]={
{"afrobank-fm","AfroBank FM","Afrobeats · amapiano"},
{"back-country-radio","Back Country Radio","Country"},
{"circoloco-records-radio","CircoLoco Records Radio","House · techno"},
{"cocoteo-fm","Cocoteo FM","Reggaeton · Latin"},
{"dirty-south-classics","Dirty South Classics","Southern hip-hop"},
{"emotion-98-3","Emotion 98.3","Soft rock · ballads"},
{"flash-fm","Flash FM","Pop"},
{"gloriana-blues-radio","Gloriana Blues Radio","Blues"},
{"honey-fm","Honey FM","R&B · soul"},
{"kaleidoscope-fm","Kaleidoscope FM","International · covers"},
{"leonida-jazz-radio","Leonida Jazz Radio","Jazz fusion"},
{"radio-la-ola","Radio La Ola","Salsa · Latin classics"},
{"radio-on-u","Radio On-U","Dub · reggae"},
{"stockyard-fm","Stockyard FM","Indie dance · electronic"},
{"symphony-fm","Symphony FM","Classical"},
{"the-chamber-106-6","The Chamber 106.6","Heavy metal"},
{"the-vault-radio","The Vault Radio","Underground house · techno"},
{"the-wipe","The Wipe","Talk"},
{"thrillbilly-mud-club-radio","Thrillbilly Mud Club Radio","Nu metal · rock"},
{"v-rock","V-Rock","Classic rock"},
{"vcrr","VCRR","Hip-hop · trap"},
{"vice-city-fm","Vice City FM","80s pop · new wave"},
{"wktt","WKTT","Talk"},
{"worldwide-fm","Worldwide FM","Jazz · soul · global"}
};

static const GtaVSrc GTA_VI_SRC[]={
{"afrobank-fm",IA_VI,"GTA 6 - AfroBank FM  Full Radio Station (31 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,8754},
{"back-country-radio",IA_VI,"GTA 6 - Back Country Radio  Full Radio Station (24 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,5064},
{"circoloco-records-radio",IA_VI,"GTA 6 - Circoloco Records Radio Full Radio Station (27 Songs).mp3",nullptr,nullptr,6444},
{"cocoteo-fm",IA_VI,"GTA 6 - Cocoteo FM  Full Radio Station (30 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,6548},
{"dirty-south-classics",IA_VI,"GTA 6 - Dirty South Classics  Full Radio Station (39 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,10453},
{"emotion-98-3",IA_VI,"GTA 6 - Emotion 98.3  Full Radio Station (34 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,10961},
{"flash-fm",IA_VI,"GTA 6 - Flash FM  Full Radio Station (80 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,20702},
{"gloriana-blues-radio",IA_VI,"GTA 6 - Gloriana Blues Radio  Full Radio Station (23 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,5383},
{"honey-fm",IA_VI,"GTA 6 - Honey FM  Full Radio Station (55 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,19777},
{"kaleidoscope-fm",IA_VI,"GTA 6 - Kaleidoscope FM  Full Radio Station (18 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,5265},
{"leonida-jazz-radio",IA_VI,"GTA 6 - Leonida Jazz Radio  Full Radio Station (36 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,10784},
{"radio-la-ola",IA_VI,"GTA 6 - Radio La Ola  Full Radio Station (25 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,7852},
{"radio-on-u",IA_VI,"GTA 6 - Radio On-U  Full Radio Station (16 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,3005},
{"stockyard-fm",IA_VI,"GTA 6 - Stockyard FM  Full Radio Station (37 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,9492},
{"symphony-fm",IA_VI,"GTA 6 - Symphony FM  Full Radio Station (16 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,8230},
{"the-chamber-106-6",IA_VI,"GTA 6 - The Chamber 106.6  Full Radio Station (22 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,6641},
{"the-vault-radio",IA_VI,"GTA 6 - The Vault Radio  Full Radio Station (23 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,5015},
{"the-wipe",IA_VI,"GTA 6 - The Wipe  Full Radio Station - GTA 6 Radio.mp3",nullptr,nullptr,12019},
{"thrillbilly-mud-club-radio",IA_VI,"GTA 6 - Thrillbilly Mud Club Radio  Full Radio Station (20 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,5021},
{"v-rock",IA_VI,"GTA 6 - V-Rock  Full Radio Station (69 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,19235},
{"vcrr",IA_VI,"GTA 6 - VCRR  Full Radio Station (80 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,16107},
{"vice-city-fm",IA_VI,"GTA 6 - Vice City FM  Full Radio Station (72 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,22136},
{"wktt",IA_VI,"GTA 6 - WKTT  Full Radio Station - GTA 6 Radio.mp3",nullptr,nullptr,6306},
{"worldwide-fm",IA_VI,"GTA 6 - Worldwide FM  Full Radio Station (39 Songs) - GTA 6 Radio.mp3",nullptr,nullptr,7232}
};

static const GtaVSrc* gtaVISrc(const std::string& slug){
    for(auto& s:GTA_VI_SRC)if(slug==s.slug)return &s;
    return nullptr;
}
struct IaMeta{ int64_t size=0,len=0; };
static std::mutex gIaMtx;
static std::map<std::string,std::string> gIaBase;
static std::map<std::string,IaMeta>      gIaFiles;
static std::atomic<bool> gIaReady{false},gIaBusy{false};

static std::string iaItem(const std::string& base){
    size_t p=base.find("/items/"); if(p==std::string::npos)return "";
    p+=7; size_t q=base.find('/',p);
    return q==std::string::npos?base.substr(p):base.substr(p,q-p);
}
static int64_t iaLen(const std::string& s){
    if(s.empty())return 0;
    if(s.find(':')==std::string::npos)return (int64_t)atof(s.c_str());
    long long h=0,m=0,sec=0;
    int n=sscanf(s.c_str(),"%lld:%lld:%lld",&h,&m,&sec);
    if(n==3)return h*3600+m*60+sec;
    if(n==2)return h*60+m;
    return 0;
}
static void iaParse(const std::string& item,const std::string& body){
    J j(body); if(!j.eat('{'))return;
    std::string k,server,dir;
    while(objNext(j,k)){
        if(k=="server")server=j.str();
        else if(k=="dir")dir=j.str();
        else if(k=="files"){
            if(!j.eat('['))break;
            while(arrNext(j)){
                if(!j.eat('{'))break;
                std::string fk,name,sz,ln;
                while(objNext(j,fk)){
                    if(fk=="name")name=j.str();
                    else if(fk=="size"){ sz=j.at('"')?j.str():std::to_string((int64_t)j.num()); }
                    else if(fk=="length"){ ln=j.at('"')?j.str():std::to_string((int64_t)j.num()); }
                    else j.skip();
                }
                j.eat('}');
                if(!name.empty()){
                    IaMeta m; m.size=atoll(sz.c_str()); m.len=iaLen(ln);
                    std::lock_guard<std::mutex> lk(gIaMtx);
                    gIaFiles[item+"\n"+name]=m;
                }
                if(!j.ok)break;
            }
            j.eat(']');
        } else j.skip();
    }
    if(!server.empty()&&!dir.empty()){
        std::lock_guard<std::mutex> lk(gIaMtx);
        gIaBase[item]="http://"+server+dir+"/";
    }
}
static void iaSave(){
    std::lock_guard<std::mutex> lk(gIaMtx);
    std::string o="{\"base\":{"; bool f=true;
    for(auto&kv:gIaBase){ if(!f)o+=","; f=false; o+="\""+jsonEsc(kv.first)+"\":\""+jsonEsc(kv.second)+"\""; }
    o+="},\"files\":{"; f=true;
    for(auto&kv:gIaFiles){ if(!f)o+=","; f=false;
        o+="\""+jsonEsc(kv.first)+"\":["+std::to_string(kv.second.size)+","+std::to_string(kv.second.len)+"]"; }
    o+="}}";
    writeFile(gCacheDir+"ia_meta.json",o);
}
static void iaLoad(){
    std::string body; if(!readFile(gCacheDir+"ia_meta.json",body))return;
    J j(body); if(!j.eat('{'))return;
    std::string k;
    while(objNext(j,k)){
        if(k=="base"){
            if(!j.eat('{'))break; std::string bk;
            while(objNext(j,bk)){ std::string v=j.str(); std::lock_guard<std::mutex> lk(gIaMtx); gIaBase[bk]=v; }
            j.eat('}');
        } else if(k=="files"){
            if(!j.eat('{'))break; std::string fk;
            while(objNext(j,fk)){
                if(j.eat('[')){
                    IaMeta m; m.size=(int64_t)j.num();
                    if(arrNext(j))m.len=(int64_t)j.num();
                    while(arrNext(j))j.skip(); j.eat(']');
                    std::lock_guard<std::mutex> lk(gIaMtx); gIaFiles[fk]=m;
                } else j.skip();
            }
            j.eat('}');
        } else j.skip();
    }
    std::lock_guard<std::mutex> lk(gIaMtx);
    gIaReady=!gIaBase.empty();
}
static bool iaHasVI(){
    std::lock_guard<std::mutex> lk(gIaMtx);
    return gIaBase.count("gta-6-radio-audio-bk-20261124-v2")>0;
}
static void iaFetch(){
    if(gIaBusy.exchange(true))return;
    const char* items[]={"Radio-files-pause-menu","radio-for-my-website-mako",
                         "gta-5-radio-songs_202603","gta-6-radio-audio-bk-20261124-v2"};
    for(const char* it:items){
        std::string err;
        std::string body=httpGet(std::string("https://archive.org/metadata/")+it,err,12,false,true,UA_APP);
        if(!body.empty())iaParse(it,body);
    }
    iaSave();
    { std::lock_guard<std::mutex> lk(gIaMtx); gIaReady=!gIaBase.empty(); }
    gIaBusy=false;
}

static int64_t gtaVPhase(const std::string& mount){ return (int64_t)(pcHash(mount)%1000000); }

static bool gtaVCandidates(const std::string& mount,std::vector<std::string>& urls,
                           std::vector<int64_t>& offs,std::vector<int64_t>& secs,int64_t& durOut){
    const GtaVSrc* s=nullptr;
    if(mount.rfind("5/",0)==0)s=gtaVSrc(mount.substr(2));
    else if(mount.rfind("6/",0)==0)s=gtaVISrc(mount.substr(2));
    if(!s)return false;
    int64_t now=(int64_t)time(nullptr)+gtaVPhase(mount);
    durOut=s->seconds;
    auto add=[&](const char* base,const char* file){
        if(!base||!file||!file[0])return;
        std::string item=iaItem(base),url; int64_t size=0,len=0;
        {
            std::lock_guard<std::mutex> lk(gIaMtx);
            auto b=gIaBase.find(item);
            url=(b!=gIaBase.end()?b->second:std::string(base))+urlEsc(file);
            auto m=gIaFiles.find(item+"\n"+file);
            if(m!=gIaFiles.end()){ size=m->second.size; len=m->second.len; }
        }
        if(len<=0)len=s->seconds;
        int64_t off=((now%len)+len)%len, byte=0;
        if(size>0&&len>0){
            byte=off*size/len;
            if(byte>size-65536)byte=size-65536;
            if(byte<0)byte=0;
        }
        urls.push_back(url); offs.push_back(byte); secs.push_back(off);
    };
    add(s->item,s->file);
    add(s->item2,s->file2);
    return !urls.empty();
}
static int gtaLocalOffsetMin(){
    time_t n=time(nullptr); tm l=tmLocal(n),g=tmUtc(n);
    int d=(l.tm_hour-g.tm_hour)*60+(l.tm_min-g.tm_min);
    int dd=l.tm_yday-g.tm_yday; if(dd>1)dd=-1; if(dd<-1)dd=1;
    return d+dd*1440;
}
struct GtaSfxBank{
    std::vector<int16_t> pcm[3];
    std::atomic<int> req{-1},cur{-1};
    std::atomic<size_t> pos{0};
    bool load(int idx,const std::string& rel,float maxSec){
        if(idx<0||idx>2)return false;
        std::string path=gtaPath(rel); if(path.empty())return false;
        std::vector<int16_t>& dst=pcm[idx]; dst.clear();
        AVFormatContext* fmt=nullptr;
        if(avformat_open_input(&fmt,path.c_str(),nullptr,nullptr)<0||!fmt)return false;
        bool ok=false;
        if(avformat_find_stream_info(fmt,nullptr)>=0){
            const AVCodec* dec=nullptr;
            int si=av_find_best_stream(fmt,AVMEDIA_TYPE_AUDIO,-1,-1,&dec,0);
            if(si>=0&&dec){
                AVCodecContext* ctx=avcodec_alloc_context3(dec);
                if(ctx&&avcodec_parameters_to_context(ctx,fmt->streams[si]->codecpar)>=0&&avcodec_open2(ctx,dec,nullptr)>=0){
                    SwrContext* swr=swr_alloc();
                    if(swr){
#if LIBAVUTIL_VERSION_INT>=AV_VERSION_INT(57,28,100)
                        AVChannelLayout outLayout=AV_CHANNEL_LAYOUT_STEREO;
                        av_opt_set_chlayout(swr,"in_chlayout",&ctx->ch_layout,0);
                        av_opt_set_chlayout(swr,"out_chlayout",&outLayout,0);
#else
                        av_opt_set_channel_layout(swr,"in_channel_layout",ctx->channel_layout?ctx->channel_layout:av_get_default_channel_layout(ctx->channels),0);
                        av_opt_set_channel_layout(swr,"out_channel_layout",AV_CH_LAYOUT_STEREO,0);
#endif
                        av_opt_set_int(swr,"in_sample_rate",ctx->sample_rate,0);
                        av_opt_set_sample_fmt(swr,"in_sample_fmt",ctx->sample_fmt,0);
                        av_opt_set_int(swr,"out_sample_rate",48000,0);
                        av_opt_set_sample_fmt(swr,"out_sample_fmt",AV_SAMPLE_FMT_S16,0);
                    }
                    if(swr&&swr_init(swr)>=0){
                        AVPacket* pkt=av_packet_alloc(); AVFrame* fr=av_frame_alloc();
                        std::vector<uint8_t> buf(48000*4);
                        size_t cap=(size_t)(maxSec*48000.f)*2;
                        if(pkt&&fr){
                            while(dst.size()<cap&&av_read_frame(fmt,pkt)>=0){
                                if(pkt->stream_index==si){
                                    avcodec_send_packet(ctx,pkt);
                                    while(avcodec_receive_frame(ctx,fr)==0&&dst.size()<cap){
                                        uint8_t* d=buf.data();
                                        int n=swr_convert(swr,&d,48000,(const uint8_t**)fr->extended_data,fr->nb_samples);
                                        if(n>0){
                                            const int16_t* samples=(const int16_t*)buf.data();
                                            for(int i=0;i<n*2&&dst.size()<cap;i++)dst.push_back(samples[i]);
                                        }
                                    }
                                }
                                av_packet_unref(pkt);
                            }
                        }
                        if(fr)av_frame_free(&fr);
                        if(pkt)av_packet_free(&pkt);
                        ok=!dst.empty();
                    }
                    swr_free(&swr);
                }
                if(ctx)avcodec_free_context(&ctx);
            }
        }
        avformat_close_input(&fmt);
        return ok;
    }
    void trigger(int i){if(i<0||i>2||pcm[i].empty())return;pos.store(0);cur.store(i);req.store(i);}
    void mix(int16_t* o,int frames){
        int r=req.exchange(-1); if(r>=0){cur.store(r);pos.store(0);}
        int c=cur.load(); if(c<0||c>2)return;
        const std::vector<int16_t>& p=pcm[c]; size_t k=pos.load();
        for(int i=0;i<frames;i++){
            if(k+1>=p.size()){cur.store(-1);break;}
            for(int ch=0;ch<2;ch++){
                int add=(int)((int)p[k+ch]*.5f),idx=i*2+ch,v=o[idx]+add;
                o[idx]=(int16_t)(v>32767?32767:(v<-32768?-32768:v));
            }
            k+=2;
        }
        pos.store(k);
    }
};
static GtaSfxBank gGtaSfx;
static void gtaSfxMix(int16_t* o,int frames){gGtaSfx.mix(o,frames);}
namespace GtaIcon{
static void wheel(Batch& b,float x,float y,float s,SDL_Color c,SDL_Color acc){
    b.ring(x,y,s*.78f,2.f,c,20);
    for(int i=0;i<8;i++){float a=i*(float)M_PI/4+.19f;b.line(x+cosf(a)*s*.34f,y+sinf(a)*s*.34f,x+cosf(a)*s*.72f,y+sinf(a)*s*.72f,1.6f,alpha(c,150));}
    b.ring(x,y,s*.30f,1.8f,acc,14);b.circle(x,y,s*.10f,acc,8);
}
}

struct Row {
    int kind=0; std::string title,sub,place,country,placeId; Channel ch;
    float lat=0,lon=0,y=0,h=0; int sel=-1,setting=-1,slot=-1;
};
struct Piece { SDL_Texture* tex=nullptr; int w=0,h=0; };
struct Lab { std::vector<Piece> p; int w=0,h=0; Uint32 use=0; };
static SDL_Color skyTint(float hour){
    struct A { float h; SDL_Color c; };
    const A a[5]={{0.f,Pal::nightTint},{5.5f,Pal::nightTint},{7.5f,Pal::dawnTint},{12.f,Pal::dayTint},{18.5f,Pal::duskTint}};
    if(hour>=18.5f)return mixc(Pal::duskTint,Pal::nightTint,clampf((hour-18.5f)/3.5f,0,1));
    for(int i=0;i<4;i++)if(hour>=a[i].h&&hour<=a[i+1].h)
        return mixc(a[i].c,a[i+1].c,(hour-a[i].h)/(a[i+1].h-a[i].h));
    return Pal::dayTint;
}

namespace HomeIcon {
static void frame(Batch& b,float x0,float y0,float w,float h,float r,SDL_Color c){
    b.rrect2(x0,y0,w,h,r,r*.8f,alpha(c,14));
    b.line(x0+r,y0,x0+w-r,y0,2.f,c);
    b.line(x0+r,y0+h,x0+w-r,y0+h,2.f,c);
    b.line(x0,y0+r,x0,y0+h-r*.8f,2.f,c);
    b.line(x0+w,y0+r,x0+w,y0+h-r*.8f,2.f,c);
    b.ring(x0+w-r,y0+r,r,2.f,c,6,-(float)M_PI/2,0.f);
    b.ring(x0+w-r,y0+h-r*.8f,r*.8f,2.f,c,6,0.f,(float)M_PI/2);
    b.ring(x0+r,y0+h-r*.8f,r*.8f,2.f,c,6,(float)M_PI/2,(float)M_PI);
    b.ring(x0+r,y0+r,r,2.f,c,6,(float)M_PI,(float)M_PI*1.5f);
}
static void radio(Batch& b,float x,float y,float s,SDL_Color c,SDL_Color acc){

    float w=s*1.66f,h=s*1.02f;
    float axr=x+w*.36f;
    float aby=y+h*.5f+s*.20f-((s*.54f-s*.66f)+h*.5f)*.5f;
    float x0=x-w*.5f,y0=aby-h*.5f+s*.20f;
    float ay=y0-s*.66f;
    b.line(x0+w*.62f,y0+s*.02f,axr,ay,2.4f,c);
    b.circle(axr,ay,2.8f,acc,8);
    b.ring(axr,ay,s*.34f,1.8f,alpha(acc,140),10,-2.6f,-.5f);
    b.ring(axr,ay,s*.54f,1.6f,alpha(acc,80),10,-2.6f,-.5f);
    frame(b,x0,y0,w,h,s*.16f,c);
    for(int i=0;i<4;i++){ float xx=x0+s*.24f+i*s*.15f;
        b.line(xx,y0+s*.24f,xx,y0+h-s*.24f,2.f,alpha(c,140)); }
    float dx=x0+w-s*.40f,dy=y0+h*.5f;
    b.ring(dx,dy,s*.26f,2.f,acc,16);
    b.line(dx,dy,dx+s*.13f,dy-s*.17f,2.f,acc);
    b.circle(dx,dy,1.8f,acc,6);
}
static void passport(Batch& b,float x,float y,float s,SDL_Color c,SDL_Color acc){
    float w=s*1.22f,h=s*1.58f,x0=x-w/2,y0=y-h/2;
    frame(b,x0,y0,w,h,s*.14f,c);
    b.line(x0+s*.16f,y0+s*.10f,x0+s*.16f,y0+h-s*.10f,1.6f,alpha(c,90));
    float gx=x+s*.06f,gy=y-s*.20f;
    b.ring(gx,gy,s*.34f,2.f,acc,18);
    b.line(gx-s*.34f,gy,gx+s*.34f,gy,1.4f,alpha(acc,150));
    b.ring(gx,gy,s*.17f,1.3f,alpha(acc,150),12);
    b.line(x-s*.28f,y+s*.44f,x+s*.34f,y+s*.44f,2.f,alpha(c,150));
    b.line(x-s*.28f,y+s*.62f,x+s*.14f,y+s*.62f,2.f,alpha(c,100));
}
static void gta(Batch& b,float x,float y,float s,SDL_Color c,SDL_Color acc){
    GtaIcon::wheel(b,x,y,s*.92f,c,acc);
    b.line(x-s*.62f,y+s*.86f,x+s*.62f,y+s*.86f,2.f,alpha(c,150));
    b.circle(x-s*.30f,y+s*.86f,2.2f,acc,6);
    b.circle(x+s*.30f,y+s*.86f,2.2f,acc,6);
}
}

struct HomeMenu {
    static constexpr int N=3;
    static constexpr float GAP=300.f;
    struct Card {
        float cx=0,cy=0,w=224,h=200,ph=0;
        float lift=0,liftV=0,sc=.93f,scV=0,push=0,pushV=0;
        float tx=0,txV=0,ty=0,tyV=0,glow=0;
        const char* name=""; int icon=0;
    };
    Card c[N];
    int sel=1; float intro=0; bool latch=false;

    void init(){
        const char* nm[N]={"World Passport","Radio","GTA Radio"};
        for(int i=0;i<N;i++){
            Card& k=c[i];
            k.cx=W/2.f+(i-(N-1)*.5f)*GAP;
            k.cy=250.f; k.ph=i*2.1f;
            k.name=nm[i]; k.icon=i;
            k.glow=(i==sel)?1.f:0.f;
            k.ty=(i<sel)?-.13f:((i>sel)?.13f:0.f);
        }
    }

    static void spring(float& x,float& v,float tgt,float k,float d,float dt){
        v+=(tgt-x)*k*dt; v*=powf(d,dt); x+=v*dt;
    }
    void move(int d){
        int n=(sel+d+N)%N; if(n==sel)return;
        Card& a=c[n]; Card& p=c[sel];
        a.liftV-=115.f; a.scV+=1.05f; a.tyV+=d*1.9f; a.txV-=.8f; a.pushV+=d*72.f;
        p.liftV+=52.f;  p.scV-=.65f;  p.tyV-=d*1.3f; p.pushV+=d*26.f;
        sel=n;
    }
    void update(float dt,bool active){
        intro=std::min(1.f,intro+dt*1.45f);
        for(int i=0;i<N;i++){
            Card& k=c[i]; bool on=active&&(i==sel);
            spring(k.lift,k.liftV,on?-16.f:0.f,170.f,2e-5f,dt);
            spring(k.sc,k.scV,on?1.06f:.93f,150.f,4e-5f,dt);
            spring(k.push,k.pushV,0.f,120.f,1e-4f,dt);
            spring(k.tx,k.txV,on?-.05f:.05f,90.f,3e-4f,dt);
            spring(k.ty,k.tyV,on?0.f:((i<sel)?-.13f:.13f),90.f,3e-4f,dt);
            k.glow=lerpf(k.glow,on?1.f:0.f,1-powf(.0025f,dt));
        }
    }

    struct Xf {
        float cx,cy,sc,cX,sX,cY,sY,cZ,sZ;
        void set(float cx_,float cy_,float sc_,float ax,float ay,float az){
            cx=cx_;cy=cy_;sc=sc_;
            cX=cosf(ax);sX=sinf(ax);cY=cosf(ay);sY=sinf(ay);cZ=cosf(az);sZ=sinf(az);
        }
        Vec2 p(float lx,float ly,float lz) const {
            float x=lx*sc,y=ly*sc,z=lz*sc;
            float x1=x*cZ-y*sZ,y1=x*sZ+y*cZ;
            float y2=y1*cX-z*sX,z2=y1*sX+z*cX;
            float x3=x1*cY+z2*sY,z3=-x1*sY+z2*cY;
            float k=620.f/(620.f+z3);
            return {cx+x3*k,cy+y2*k};
        }
    };
    static void outline(float w,float h,float r,std::vector<Vec2>& o){
        o.clear();
        const float cx[4]={w/2-r,w/2-r,-w/2+r,-w/2+r};
        const float cy[4]={-h/2+r,h/2-r,h/2-r,-h/2+r};
        const float a0[4]={-(float)M_PI/2,0.f,(float)M_PI/2,(float)M_PI};
        for(int k=0;k<4;k++)for(int i=0;i<=4;i++){
            float a=a0[k]+(float)M_PI/2*i/4.f;
            o.push_back({cx[k]+cosf(a)*r,cy[k]+sinf(a)*r});
        }
    }
    static void softEllipse(Batch& b,float cx,float cy,float rx,float ry,SDL_Color c,int seg=24){
        if(c.a==0||rx<=0||ry<=0)return;
        int base=b.vert(cx,cy,c);
        for(int i=0;i<seg;i++){ float a=2*(float)M_PI*i/seg;
            b.vert(cx+cosf(a)*rx,cy+sinf(a)*ry,alpha(c,0)); }
        for(int i=0;i<seg;i++)b.tri(base,base+1+i,base+1+(i+1)%seg);
    }
};

static SDL_Color hsv(float h,float s,float v,int a=255){
    h=fmodf(h,360.f); if(h<0)h+=360.f;
    float c=v*s,x=c*(1.f-fabsf(fmodf(h/60.f,2.f)-1.f)),m=v-c,r=0,g=0,b=0;
    if(h<60){r=c;g=x;} else if(h<120){r=x;g=c;} else if(h<180){g=c;b=x;}
    else if(h<240){g=x;b=c;} else if(h<300){r=x;b=c;} else {r=c;b=x;}
    return C((int)((r+m)*255),(int)((g+m)*255),(int)((b+m)*255),a);
}
static uint32_t pcHash(const std::string& s){
    uint32_t h=2166136261u;
    for(unsigned char ch:s){h^=ch;h*=16777619u;}
    return h;
}
#include "gta_world.h"
struct Card {
    std::string id,city,country,cc,photoUrl;
    float lat=0,lon=0;
    int variant=0,photoState=0;
    float failT=-1e9f;
    long long ts=0;
};
struct PcPhotoRes { std::string id,url; int w=0,h=0; std::vector<uint8_t> rgba; bool ok=false; };
static float pcHue(SDL_Color c){
    float r=c.r/255.f,g=c.g/255.f,b=c.b/255.f;
    float mx=std::max(r,std::max(g,b)),mn=std::min(r,std::min(g,b)),d=mx-mn;
    if(d<1e-5f)return 0.f;
    float h;
    if(mx==r)h=60.f*fmodf((g-b)/d,6.f);
    else if(mx==g)h=60.f*((b-r)/d+2.f);
    else h=60.f*((r-g)/d+4.f);
    return h<0?h+360.f:h;
}
static float pcSat(SDL_Color c){
    float mx=std::max(c.r,std::max(c.g,c.b)),mn=std::min(c.r,std::min(c.g,c.b));
    return mx>0?(mx-mn)/mx:0.f;
}
static float pcLum(SDL_Color c){ return (c.r*.299f+c.g*.587f+c.b*.114f)/255.f; }

struct FlagPal {
    SDL_Color c[3]={C(64,104,176),C(196,84,72),C(245,245,242)};
    int n=1; bool white=false,dark=false; float whiteFrac=0; bool ok=false;
};
static std::map<std::string,FlagPal> gFlagPal;
static FlagPal pcPalFromPixels(const std::vector<uint8_t>& px,int w,int h){
    FlagPal fp;
    if(w<=0||h<=0||px.size()<(size_t)w*h*4)return fp;
    struct B{int n=0;long r=0,g=0,b=0;};
    std::vector<B> bk(4096);
    long total=0,whiteN=0,darkN=0;
    for(size_t i=0;i<(size_t)w*h;i++){
        const uint8_t* p=&px[i*4]; if(p[3]<128)continue;
        int r=p[0],g=p[1],b=p[2]; total++;
        int mx=std::max(r,std::max(g,b)),mn=std::min(r,std::min(g,b));
        float sat=mx?(mx-mn)/(float)mx:0.f,lum=(r*.299f+g*.587f+b*.114f)/255.f;
        if(sat<.14f&&lum>.80f)whiteN++;
        if(lum<.20f)darkN++;
        int id=((r>>4)<<8)|((g>>4)<<4)|(b>>4);
        B& q=bk[id]; q.n++; q.r+=r; q.g+=g; q.b+=b;
    }
    if(!total)return fp;
    fp.whiteFrac=whiteN/(float)total;
    fp.white=fp.whiteFrac>.10f;
    fp.dark=darkN/(float)total>.08f;
    std::vector<std::pair<int,SDL_Color>> v;
    for(auto& q:bk)if(q.n>0)v.push_back({q.n,C((int)(q.r/q.n),(int)(q.g/q.n),(int)(q.b/q.n))});
    std::sort(v.begin(),v.end(),[](const std::pair<int,SDL_Color>&a,const std::pair<int,SDL_Color>&b){return a.first>b.first;});
    int np=0;
    for(auto& e:v){
        SDL_Color c=e.second;
        if(pcSat(c)<.16f)continue;
        float L=pcLum(c); if(L<.08f||L>.94f)continue;
        bool dup=false;
        for(int i=0;i<np;i++){float d=fabsf(pcHue(c)-pcHue(fp.c[i]));if(d>180)d=360-d;if(d<24.f)dup=true;}
        if(dup)continue;
        fp.c[np++]=c; if(np==3)break;
    }
    if(np==0)return fp;
    fp.n=np;
    if(fp.n<2){
        if(fp.white)fp.c[1]=hsv(pcHue(fp.c[0]),std::min(1.f,pcSat(fp.c[0])*1.15f),pcLum(fp.c[0])*.55f);
        else if(fp.dark)fp.c[1]=C(38,40,48);
        else fp.c[1]=hsv(pcHue(fp.c[0])+150.f,.55f,.62f);
        fp.n=2;
    }
    if(fp.n<3){ fp.c[2]=fp.white?C(250,250,248):(fp.dark?C(30,32,40):hsv(pcHue(fp.c[0]),.30f,.88f)); fp.n=3; }
    fp.ok=true;
    return fp;
}
static const char* FLAG_TBL=
"jp=bc002d;us=3c3b6e|b22234;gb=012169|c8102e;fr=002395|ed2939;de=dd0000|ffce00;it=008c45|cd212a;"
"es=aa151b|f1bf00;pt=046a38|da291c;nl=ae1c28|21468b;be=ef3340|fdda24;ch=d52b1e;at=ed2939;"
"pl=dc143c;cz=d7141a|11457e;sk=ee1c25|0b4ea2;hu=cd2a3e|436f4d;ro=fcd116|ce1126;bg=00966e|d62612;"
"gr=0d5eaf;tr=e30a17;ru=0039a6|d52b1e;ua=0057b7|ffd700;by=ce1720|007c30;lt=fdb913|006a44;"
"lv=9e3039;ee=0072ce;fi=003580;se=006aa7|fecc00;no=ba0c2f|00205b;dk=c60c30;is=02529c|dc1e35;"
"ie=169b62|ff883e;ca=d80621;au=00247d|e4002b;nz=00247d|cc142b;cn=de2910|ffde00;kr=003478|c60c30;"
"tw=000095|fe0000;hk=de2910;in=ff9933|138808;pk=01411c;bd=006a4e|f42a41;lk=ffbe29|8d153a;"
"np=dc143c|003893;th=a51931|2d2a4a;vn=da251d;id=ce1126;my=010066|cc0001;sg=ed2939;ph=0038a8|ce1126;"
"kh=032ea1|e00025;mm=fecb00|ea2839;mn=c4272f|015197;kz=00afca|fec50c;uz=0099b5|ce1126;ge=ff0000;"
"am=d90012|0033a0;az=00b5e2|ef3340;il=0038b8;sa=006c35;ae=00732f|ff0000;eg=ce1126;ma=c1272d|006233;"
"dz=006233|d21034;tn=e70013;ly=239e46;ng=008751;ke=bb0000|006600;za=007a4d|ffb612;gh=ce1126|fcd116;"
"et=078930|fcdd09;tz=1eb53a|00a3dd;ug=fcdc04|d90000;zm=198a00|ef7d00;zw=006400|ffd200;ao=cc092f;"
"cm=007a5e|ce1126;sn=00853f|fdef42;ci=ff8200|009a44;ml=14b53a|fcd116;bf=ef2b2d|009e49;ne=e05206|0db02b;"
"td=002664|fecb00;mx=006847|ce1126;br=009c3b|ffdf00;ar=74acdf|f6b40e;cl=0039a6|d52b1e;co=fcd116|003893;"
"pe=d91023;ve=ffcc00|cf142b;ec=ffdd00|034ea2;bo=d52b1e|007934;py=d52b1e|0038a8;uy=0038a8|fcd116;"
"cr=002b7f|ce1126;pa=005293|d21034;gt=4997d0;cu=002a8f|cf142b;do=002d62|ce1126;jm=009b3a|fed100;"
"ht=00209f|d21034;hn=0073cf;ni=0067c6;sv=0047ab;bz=003f87|ce1126;tt=ce1126;bb=00267f|ffc726;"
"hr=ff0000|171796;rs=c6363c|0c4076;ba=002395|fecb00;si=005da4|ed1c24;mk=d20000|ffe600;al=e41e20;"
"me=c40308|d3ae3b;mt=cf142b;cy=d57800|4e5b31;lu=ed2939|00a1de;mc=ce1126;ad=10069f|fedf00;sm=5eb6e4;"
"li=002b7f|ce1126;gl=cf142b;fo=0065bd|ed2939";
static FlagPal pcPalFromTable(const std::string& iso){
    static std::map<std::string,FlagPal> m;
    if(m.empty()){
        auto hex=[](const std::string& s)->SDL_Color{
            unsigned v=0; for(char ch:s){ v<<=4;
                v+=(ch>='0'&&ch<='9')?ch-'0':((ch|32)-'a'+10); }
            return C((v>>16)&255,(v>>8)&255,v&255);
        };
        std::string s=FLAG_TBL; size_t i=0;
        while(i<s.size()){
            size_t j=s.find(';',i); if(j==std::string::npos)j=s.size();
            std::string kv=s.substr(i,j-i); size_t e=kv.find('=');
            if(e!=std::string::npos){
                std::string key=kv.substr(0,e),val=kv.substr(e+1);
                size_t p=val.find('|');
                FlagPal fp; fp.ok=true;
                fp.c[0]=hex(p==std::string::npos?val:val.substr(0,p));
                fp.c[1]=p==std::string::npos?hsv(pcHue(fp.c[0])+150.f,.55f,.62f):hex(val.substr(p+1));
                m[key]=fp;
            }
            i=j+1;
        }
    }
    auto it=m.find(iso);
    return it==m.end()?FlagPal{}:it->second;
}
struct PcTheme { SDL_Color paper,ink,a1,a2,a3,at,tape; };
static PcTheme pcTheme(const Card& c){
    std::string iso=lowerStr(c.cc.empty()?isoForCountry(c.country):c.cc);
    FlagPal fp;
    auto it=gFlagPal.find(iso);
    if(it!=gFlagPal.end()&&it->second.ok)fp=it->second;
    else fp=pcPalFromTable(iso);
    PcTheme t;
    float h1=pcHue(fp.c[0]);
    float amt=.16f+.30f*(1.f-clampf(fp.whiteFrac,0.f,1.f));
    t.paper=mixc(C(250,247,240),hsv(h1,.34f,.97f),amt);
    t.ink=fp.dark?mixc(C(26,28,34),fp.c[0],.12f):hsv(h1,.55f,.19f);
    t.a1=fp.c[0]; t.a2=fp.c[1]; t.a3=fp.c[2];
    t.at=pcLum(t.a1)>.62f?hsv(h1,std::max(.5f,pcSat(t.a1)),.50f):t.a1;
    t.tape=alpha(mixc(t.a2,C(255,255,255),.40f),160);
    return t;
}
static void pcShadow(Batch& b,float cx,float cy,float rx,float ry,SDL_Color c,int seg=26){
    if(c.a==0||rx<=0||ry<=0)return;
    int base=b.vert(cx,cy,c);
    for(int i=0;i<seg;i++){float a=2*(float)M_PI*i/seg;b.vert(cx+cosf(a)*rx,cy+sinf(a)*ry,alpha(c,0));}
    for(int i=0;i<seg;i++)b.tri(base,base+1+i,base+1+(i+1)%seg);
}
namespace PcIcon {
static void card(Batch& b,float x,float y,float s,SDL_Color c){
    b.rrect2(x-s*.8f,y-s*.6f,s*1.6f,s*1.2f,2,2,alpha(c,20));
    b.line(x-s*.8f,y-s*.6f,x+s*.8f,y-s*.6f,1.6f,c);
    b.line(x-s*.8f,y+s*.6f,x+s*.8f,y+s*.6f,1.6f,c);
    b.line(x-s*.8f,y-s*.6f,x-s*.8f,y+s*.6f,1.6f,c);
    b.line(x+s*.8f,y-s*.6f,x+s*.8f,y+s*.6f,1.6f,c);
    b.rrect2(x+s*.26f,y-s*.44f,s*.38f,s*.34f,1,1,alpha(c,120));
    b.line(x-s*.58f,y+s*.06f,x+s*.08f,y+s*.06f,1.4f,alpha(c,150));
    b.line(x-s*.58f,y+s*.32f,x-s*.12f,y+s*.32f,1.4f,alpha(c,110));
}
static void leaf(Batch& b,float x,float y,float s,SDL_Color c){
    b.circle(x,y,s*.5f,c,10);
    b.tri(b.vert(x-s*.5f,y,c),b.vert(x+s*.5f,y,c),b.vert(x,y-s*.95f,c));
    b.line(x,y+s*.45f,x,y-s*.55f,1.2f,alpha(C(0,0,0),50));
}
}

static bool decodeImageFit(const std::string& body,int maxDim,std::vector<uint8_t>& out,int& ow,int& oh){
    if(body.size()<64||maxDim<=0)return false;
	    if(decodeImageStb(body,maxDim,out,ow,oh))return true;
    const unsigned char* d=(const unsigned char*)body.data();
    AVCodecID cid=AV_CODEC_ID_NONE;
    if(d[0]==0x89&&d[1]=='P'&&d[2]=='N'&&d[3]=='G')cid=AV_CODEC_ID_PNG;
    else if(d[0]==0xFF&&d[1]==0xD8)cid=AV_CODEC_ID_MJPEG;
    else if(d[0]=='R'&&d[1]=='I'&&d[2]=='F'&&d[3]=='F'&&body.size()>12&&d[8]=='W')cid=AV_CODEC_ID_WEBP;
    if(cid==AV_CODEC_ID_NONE)return false;
    const AVCodec* dec=avcodec_find_decoder(cid); if(!dec)return false;
    AVCodecContext* ctx=avcodec_alloc_context3(dec); if(!ctx)return false;
    bool ok=false;
    if(avcodec_open2(ctx,dec,nullptr)>=0){
        std::vector<uint8_t> buf(body.begin(),body.end());
        buf.resize(buf.size()+AV_INPUT_BUFFER_PADDING_SIZE,0);
        AVPacket* pkt=av_packet_alloc(); AVFrame* fr=av_frame_alloc();
        if(pkt&&fr){
            pkt->data=buf.data(); pkt->size=(int)body.size();
            if(avcodec_send_packet(ctx,pkt)>=0&&avcodec_receive_frame(ctx,fr)>=0
               &&fr->width>0&&fr->height>0&&(int64_t)fr->width*fr->height<=6000000){
                int sw=fr->width,sh=fr->height;
                float sc=std::min(1.f,std::min(maxDim/(float)sw,maxDim/(float)sh));
                ow=std::max(1,(int)(sw*sc)); oh=std::max(1,(int)(sh*sc));
                SwsContext* s=sws_getContext(sw,sh,(AVPixelFormat)fr->format,ow,oh,AV_PIX_FMT_RGBA,
                                             SWS_BILINEAR,nullptr,nullptr,nullptr);
                if(s){
                    out.resize((size_t)ow*oh*4);
                    uint8_t* dp[4]={out.data(),nullptr,nullptr,nullptr}; int ds[4]={ow*4,0,0,0};
                    sws_scale(s,fr->data,fr->linesize,0,sh,dp,ds);
                    sws_freeContext(s); ok=true;
                }
            }
        }
        if(fr)av_frame_free(&fr);
        if(pkt)av_packet_free(&pkt);
    }
    avcodec_free_context(&ctx);
    return ok;
}

static void gtaTrimAlpha(std::vector<uint8_t>& px,int& w,int& h){
    if(w<=0||h<=0||(size_t)w*(size_t)h*4>px.size())return;
    int x0=w,y0=h,x1=-1,y1=-1;
    for(int y=0;y<h;y++)for(int x=0;x<w;x++){
        if(px[((size_t)y*w+x)*4+3]>10){
            if(x<x0)x0=x;
            if(x>x1)x1=x;
            if(y<y0)y0=y;
            if(y>y1)y1=y;
        }
    }
    if(x1<0||y1<0)return;
    int nw=x1-x0+1,nh=y1-y0+1;
    if(nw==w&&nh==h)return;
    std::vector<uint8_t> out((size_t)nw*nh*4);
    for(int y=0;y<nh;y++)
        memcpy(out.data()+(size_t)y*nw*4,px.data()+((size_t)(y+y0)*w+x0)*4,(size_t)nw*4);
    px.swap(out);w=nw;h=nh;
}
static std::string pcLang(const std::string& cc){
    static const char* T=
    "jp=ja;cn=zh;tw=zh;hk=zh;kr=ko;ru=ru;ua=uk;by=be;kz=kk;ge=ka;am=hy;az=az;tr=tr;gr=el;il=he;"
    "sa=ar;eg=ar;ae=ar;iq=ar;sy=ar;lb=ar;jo=ar;ye=ar;om=ar;qa=ar;kw=ar;bh=ar;ly=ar;ma=ar;dz=ar;"
    "tn=ar;sd=ar;ir=fa;af=fa;pk=ur;in=hi;bd=bn;lk=si;np=ne;th=th;vn=vi;id=id;my=my;kh=km;mn=mn;"
    "de=de;at=de;ch=de;fr=fr;be=fr;lu=fr;mc=fr;es=es;mx=es;ar=es;cl=es;co=es;pe=es;ve=es;ec=es;"
    "bo=es;py=es;uy=es;cr=es;pa=es;gt=es;do=es;cu=es;hn=es;ni=es;sv=es;pr=es;it=it;sm=it;pt=pt;"
    "br=pt;ao=pt;mz=pt;cv=pt;nl=nl;sr=nl;pl=pl;cz=cs;sk=sk;hu=hu;ro=ro;md=ro;bg=bg;rs=sr;hr=hr;"
    "ba=bs;si=sl;mk=mk;al=sq;dk=da;no=nb;se=sv;fi=fi;is=is;ee=et;lv=lv;lt=lt;ke=sw;tz=sw;na=af";
    static std::map<std::string,std::string> m;
    if(m.empty()){
        std::string s=T; size_t i=0;
        while(i<s.size()){ size_t j=s.find(';',i); if(j==std::string::npos)j=s.size();
            std::string kv=s.substr(i,j-i); size_t e=kv.find('=');
            if(e!=std::string::npos)m[kv.substr(0,e)]=kv.substr(e+1);
            i=j+1; }
    }
    auto it=m.find(lowerStr(cc));
    return it==m.end()?"":it->second;
}
static bool pcGoodPhoto(const std::string& u,const std::string& title){
    std::string s=lowerStr(u+" "+title);
    const char* bad[]={"flag_of","flag of","coat_of","coat of","arms","logo","locator","_map",
                       "map_of","map of","blank","icon","stub","question_book","diagram","chart",
                       ".svg","commons-logo","emblem","seal_of","wordmark","symbol_of"};
    for(const char* b:bad)if(s.find(b)!=std::string::npos)return false;
    return true;
}

static std::string pcPickThumb(const std::string& body){
    J j(body); if(!j.eat('{'))return "";
    std::string k,best; int bestIdx=99999;
    while(objNext(j,k)){
        if(k!="query"){j.skip();continue;}
        if(!j.eat('{'))break;
        std::string qk;
        while(objNext(j,qk)){
            if(qk!="pages"){j.skip();continue;}
            if(!j.eat('{'))break;
            std::string pk;
            while(objNext(j,pk)){
                if(!j.at('{')){j.skip();continue;}
                if(!j.eat('{'))break;
                std::string fk,url,title; int idx=999;
                while(objNext(j,fk)){
                    if(fk=="index")idx=(int)j.num();
                    else if(fk=="title")title=j.str();
                    else if(fk=="thumbnail"){
                        if(j.eat('{')){std::string tk;
                            while(objNext(j,tk)){ if(tk=="source")url=j.str(); else j.skip(); }
                            j.eat('}');}
                    }
                    else if(fk=="imageinfo"){
                        if(j.eat('[')){
                            if(arrNext(j)&&j.eat('{')){std::string ik;
                                while(objNext(j,ik)){ if(ik=="thumburl")url=j.str(); else j.skip(); }
                                j.eat('}');}
                            while(arrNext(j))j.skip();
                            j.eat(']');
                        }
                    }
                    else j.skip();
                }
                j.eat('}');
                if(!url.empty()&&idx<bestIdx&&pcGoodPhoto(url,title)){bestIdx=idx;best=url;}
            }
            j.eat('}');
        }
        j.eat('}');
    }
    return best;
}
static std::string pcWiki(const std::string& host,const std::string& q){
    std::string e;
    std::string u="https://"+host+"/w/api.php?action=query&format=json&redirects=1"
                  "&prop=pageimages&piprop=thumbnail&pithumbsize=900&"+q;
    std::string b=httpGet(u,e,9,false,true,UA_WIKI);
    return b.empty()?std::string():pcPickThumb(b);
}
static std::string pcFindPhoto(const Card& c,std::string& note){
    note.clear();
    std::string iso=lowerStr(c.cc.empty()?isoForCountry(c.country):c.cc);
    std::string city=c.city, country=c.country, url;
    url=pcWiki("en.wikipedia.org","titles="+urlEsc(city));
    if(!url.empty())return url;
    url=pcWiki("en.wikipedia.org","generator=search&gsrlimit=6&gsrsearch="+urlEsc(city+" "+country));
    if(!url.empty())return url;
    if(c.lat!=0||c.lon!=0){
        char g[160]; snprintf(g,sizeof g,"generator=geosearch&ggscoord=%.4f%%7C%.4f&ggsradius=12000&ggslimit=10",c.lat,c.lon);
        url=pcWiki("en.wikipedia.org",g);
        if(!url.empty())return url;
    }
    std::string lang=pcLang(iso);
    if(!lang.empty()){
        url=pcWiki(lang+".wikipedia.org","generator=search&gsrlimit=6&gsrsearch="+urlEsc(city));
        if(!url.empty())return url;
    }
    std::string e;
    std::string u="https://commons.wikimedia.org/w/api.php?action=query&format=json&generator=search"
                  "&gsrnamespace=6&gsrlimit=10&prop=imageinfo&iiprop=url&iiurlwidth=900&gsrsearch="+
                  urlEsc(city+" "+country);
    std::string b=httpGet(u,e,9,false,true,UA_WIKI);
    if(!b.empty()){ url=pcPickThumb(b); if(!url.empty())return url; }
    return "";
}
static AccountUid gAccUid{};
static bool gAccUidSet=false;
static std::string accTag(){
    if(!gAccUidSet)return "default";
    char b[24]; snprintf(b,sizeof b,"%016llx",(unsigned long long)gAccUid.uid[0]);
    return b;
}
static std::string ppPath(){ return gCacheDir+"passport_"+accTag()+".json"; }
static std::string stPath(){ return gCacheDir+"stamps_"+accTag()+".json"; }
static void accSave(){
    if(!gAccUidSet)return;
    char b[80];
    snprintf(b,sizeof b,"{\"uid\":\"%016llx%016llx\"}",
             (unsigned long long)gAccUid.uid[0],(unsigned long long)gAccUid.uid[1]);
    writeFile(gCacheDir+"account.json",b);
}
static bool accLoad(){
    std::string body;
    if(!readFile(gCacheDir+"account.json",body))return false;
    J j(body); if(!j.eat('{'))return false;
    std::string k,s;
    while(objNext(j,k)){ if(k=="uid")s=j.str(); else j.skip(); }
    if(s.size()<32)return false;
    auto hx=[&](size_t off){
        unsigned long long v=0;
        for(size_t i=off;i<off+16;i++){ char c=s[i]; v<<=4;
            v+=(c>='0'&&c<='9')?c-'0':((c|32)>='a'&&(c|32)<='f')?(c|32)-'a'+10:0; }
        return v;
    };
    gAccUid.uid[0]=hx(0); gAccUid.uid[1]=hx(16);
    gAccUidSet=(gAccUid.uid[0]||gAccUid.uid[1]);
    return gAccUidSet;
}
static void accAdoptLegacy(){
    std::string body;
    if(!fileExists(ppPath())&&readFile(gCacheDir+"passport.json",body))writeFile(ppPath(),body);
    if(!fileExists(stPath())&&readFile(gCacheDir+"stamps.json",body))writeFile(stPath(),body);
}

struct Passport {
    std::string nick="Traveler", number;
    long long issued=0, airSec=0;
    int cities=0, stations=0;
    std::vector<std::string> countries;
    std::vector<uint32_t> cityIds;
};
static Passport ppLoad(){
    Passport p; std::string body;
    if(!readFile(ppPath(),body))return p;
    J j(body); if(!j.eat('{'))return p;
    std::string k;
    while(objNext(j,k)){
        if(k=="nick")p.nick=j.str();
        else if(k=="num")p.number=j.str();
        else if(k=="issued")p.issued=(long long)j.num();
        else if(k=="air")p.airSec=(long long)j.num();
        else if(k=="cities")p.cities=(int)j.num();
        else if(k=="stations")p.stations=(int)j.num();
        else if(k=="countries"){ if(j.eat('[')){ while(arrNext(j))p.countries.push_back(j.str()); j.eat(']'); } }
        else if(k=="cityIds"){ if(j.eat('[')){ while(arrNext(j))p.cityIds.push_back((uint32_t)j.num()); j.eat(']'); } }
        else j.skip();
        if(!j.ok)break;
    }
    if(p.nick.empty())p.nick="Traveler";
    if(p.cityIds.size()>1024)p.cityIds.resize(1024);
    if(p.countries.size()>240)p.countries.resize(240);
    return p;
}
static void ppSave(const Passport& p){
    std::string o="{\"nick\":\""+jsonEsc(p.nick)+"\",\"num\":\""+jsonEsc(p.number)+"\"";
    o+=",\"issued\":"+std::to_string(p.issued);
    o+=",\"air\":"+std::to_string(p.airSec);
    o+=",\"cities\":"+std::to_string(p.cities);
    o+=",\"stations\":"+std::to_string(p.stations);
    o+=",\"countries\":[";
    for(size_t i=0;i<p.countries.size();i++){ if(i)o+=","; o+="\""+jsonEsc(p.countries[i])+"\""; }
    o+="],\"cityIds\":[";
    for(size_t i=0;i<p.cityIds.size();i++){ if(i)o+=","; o+=std::to_string(p.cityIds[i]); }
    o+="]}";
    writeFile(ppPath(),o);
}

enum { ST_ENTRY=0 };
struct Stamp {
    std::string iso,country,city,placeId,station;
    long long ts=0; int tzMin=0;
    int type=0,airSec=0,ink=0;
    uint32_t seed=0,cityHash=0,stationHash=0;
};
struct StampDB {
    std::vector<Stamp> v;
    std::map<std::string,int> sec;
    std::map<std::string,long long> last;
    std::set<std::string> done;
};
struct StampCDef { std::string iso,cont,local; int tpl=0,motif=0,date=0; };
static const char* ST_C_TBL=
"jp|AS|3|12|5|入国;cn|AS|2|13|1|入境;kr|AS|0|12|1|입국;tw|AS|3|13|1|入境;hk|AS|2|14|1|入境;"
"mn|AS|6|36|1|ИРЭЛТ;kz|AS|4|23|0|КІРУ;uz|AS|2|45|0|KIRISH;ge|AS|5|40|0|შესვლა;am|AS|5|16|0|ՄՈՒՏՔ;"
"az|AS|4|10|0|GİRİŞ;tr|AS|1|10|0|GİRİŞ;il|AS|0|22|3|כניסה;sa|AS|4|10|4|دخول;ae|AS|4|10|4|دخول;"
"eg|AS|4|9|4|دخول;ma|AF|4|10|4|دخول;dz|AF|4|10|4|دخول;tn|AF|4|10|4|دخول;ly|AF|4|10|4|دخول;"
"ir|AS|4|10|4|ورود;iq|AS|4|10|4|دخول;jo|AS|4|9|4|دخول;lb|AS|4|10|4|دخول;sy|AS|4|10|4|دخول;"
"ye|AS|4|10|4|دخول;om|AS|4|10|4|دخول;qa|AS|4|10|4|دخول;kw|AS|4|10|4|دخول;bh|AS|4|10|4|دخول;"
"in|AS|2|21|3|प्रवेश;pk|AS|2|10|3|ENTRY;bd|AS|2|17|3|প্রবেশ;lk|AS|1|18|3|ඇතුල්වීම;np|AS|6|16|3|प्रवेश;"
"bt|AS|6|13|3|ENTRY;mv|AS|0|30|3|ENTRY;af|AS|4|16|3|ENTRY;th|AS|2|13|6|เข้าเมือง;vn|AS|2|13|1|NHẬP CẢNH;"
"id|AS|1|30|3|MASUK;my|AS|2|13|3|MASUK;sg|AS|2|14|3|ENTRY;ph|AS|1|30|3|ENTRY;kh|AS|4|45|3|ចូល;"
"mm|AS|4|13|3|ENTRY;la|AS|4|13|3|ENTRY;bn|AS|4|13|3|MASUK;tl|AS|4|30|3|ENTRY;"
"de|EU|2|14|0|EINREISE;fr|EU|2|14|0|ENTRÉE;it|EU|2|14|0|INGRESSO;es|EU|2|14|0|ENTRADA;pt|EU|2|14|0|ENTRADA;"
"nl|EU|2|4|0|BINNENKOMST;be|EU|2|14|0|ENTRÉE;ch|EU|2|16|0|EINREISE;at|EU|2|14|0|EINREISE;pl|EU|2|14|0|WJAZD;"
"cz|EU|2|14|0|VSTUP;sk|EU|2|14|0|VSTUP;hu|EU|2|14|0|BELÉPÉS;ro|EU|2|14|0|INTRARE;bg|EU|2|14|0|ВХОД;"
"gr|EU|2|14|0|ΕΙΣΟΔΟΣ;hr|EU|2|14|0|ULAZ;rs|EU|2|14|0|УЛАЗ;ba|EU|2|14|0|ULAZ;si|EU|2|14|0|VSTOP;"
"mk|EU|2|14|0|ВЛЕЗ;al|EU|2|14|0|HYRJE;me|EU|2|14|0|ULAZ;mt|EU|2|14|0|DĦUL;cy|EU|2|14|0|ΕΙΣΟΔΟΣ;"
"lu|EU|2|14|0|ENTRÉE;mc|EU|2|14|0|ENTRÉE;ad|EU|2|16|0|ENTRADA;sm|EU|2|14|0|INGRESSO;li|EU|2|14|0|EINREISE;"
"ru|EU|8|37|0|ПРОПУСК;ua|EU|8|25|0|В'ЇЗД;by|EU|8|25|0|УВАЗД;md|EU|8|14|0|INTRARE;lt|EU|2|26|0|ĮVAŽIAVIMAS;"
"lv|EU|2|26|0|IECEĻOŠANA;ee|EU|2|26|0|SISENEMINE;fi|EU|2|31|0|SAAPUMINEN;se|EU|2|31|0|INRESA;"
"no|EU|2|31|0|INNREISE;dk|EU|2|31|0|INDREJSE;is|EU|2|31|0|KOMU;ie|EU|2|26|0|ENTRY;gb|EU|2|14|0|LEAVE TO ENTER;"
"us|NA|1|24|2|ADMITTED;ca|NA|1|2|2|ADMITTED;mx|NA|0|24|2|ENTRADA;gt|NA|2|29|2|ENTRADA;hn|NA|2|29|2|ENTRADA;"
"ni|NA|2|29|2|ENTRADA;sv|NA|2|29|2|ENTRADA;cr|NA|2|30|2|ENTRADA;pa|NA|2|32|2|ENTRADA;cu|NA|1|30|2|ENTRADA;"
"do|NA|1|30|2|ENTRADA;ht|NA|1|30|2|ENTRÉE;jm|NA|1|30|2|ENTRY;tt|NA|1|30|2|ENTRY;bb|NA|1|30|2|ENTRY;"
"bs|NA|1|30|2|ENTRY;bz|NA|2|32|2|ENTRY;pr|NA|1|24|2|ADMITTED;"
"br|SA|1|25|3|ENTRADA;ar|SA|1|38|3|ENTRADA;cl|SA|1|16|3|ENTRADA;co|SA|1|18|3|ENTRADA;pe|SA|1|8|3|ENTRADA;"
"ve|SA|1|30|3|ENTRADA;ec|SA|1|30|3|ENTRADA;bo|SA|1|8|3|ENTRADA;py|SA|1|25|3|ENTRADA;uy|SA|1|38|3|ENTRADA;"
"gy|SA|1|25|3|ENTRY;sr|SA|1|25|3|BINNENKOMST;"
"au|OC|1|6|2|ARRIVAL;nz|OC|1|7|2|ARRIVAL;fj|OC|1|30|2|ENTRY;pg|OC|1|34|2|ENTRY;ws|OC|1|30|2|ENTRY;"
"to|OC|1|30|2|ENTRY;pw|OC|1|30|2|ENTRY;gu|OC|1|24|2|ADMITTED;"
"ng|AF|2|50|3|ENTRY;ke|AF|2|36|3|ENTRY;tz|AF|2|16|3|ENTRY;ug|AF|2|36|3|ENTRY;za|AF|2|19|3|ENTRY;"
"gh|AF|2|50|3|ENTRY;et|AF|2|18|3|ENTRY;zm|AF|2|33|3|ENTRY;zw|AF|2|33|3|ENTRY;ao|AF|2|18|3|ENTRADA;"
"cm|AF|2|50|3|ENTRÉE;sn|AF|2|20|3|ENTRÉE;ci|AF|2|18|3|ENTRÉE;ml|AF|2|20|3|ENTRÉE;bf|AF|2|20|3|ENTRÉE;"
"ne|AF|2|35|3|ENTRÉE;td|AF|2|35|3|ENTRÉE;sd|AF|4|9|4|دخول;ss|AF|2|20|3|ENTRY;cg|AF|2|20|3|ENTRÉE;"
"mg|AF|1|25|3|ENTRÉE;mu|AF|1|30|3|ENTRY;sc|AF|1|30|3|ENTRY;cv|AF|1|30|3|ENTRADA;"
"gl|NA|2|31|0|ENTRY;fo|EU|2|31|0|ENTRY;je|EU|2|14|0|ENTRY;gi|EU|2|14|0|ENTRY;xk|EU|2|14|0|HYRJE;"
"ps|AS|4|10|4|دخول;";
static const StampCDef& stDef(const std::string& iso){
    static std::map<std::string,StampCDef> m;
    static StampCDef fb;
    if(m.empty()){
        std::string s=ST_C_TBL; size_t i=0;
        while(i<s.size()){
            size_t j=s.find(';',i); if(j==std::string::npos)j=s.size();
            std::string e=s.substr(i,j-i); i=j+1;
            std::vector<std::string> f; size_t a=0;
            while(a<=e.size()){ size_t b=e.find('|',a); if(b==std::string::npos)b=e.size();
                f.push_back(e.substr(a,b-a)); a=b+1; }
            if(f.size()<6)continue;
            StampCDef d; d.iso=f[0]; d.cont=f[1]; d.tpl=atoi(f[2].c_str());
            d.motif=atoi(f[3].c_str()); d.date=atoi(f[4].c_str()); d.local=f[5];
            m[d.iso]=d;
        }
    }
    auto it=m.find(lowerStr(iso));
    if(it!=m.end())return it->second;
    fb.iso=lowerStr(iso); fb.cont="UN"; fb.tpl=6; fb.motif=0; fb.date=0; fb.local="ENTRY";
    return fb;
}
static const char* stTypeName(int){ return "ENTRY"; }
static std::string stPort(const std::string& city,const std::string& iso){
    std::string o;
    for(unsigned char c:city){ if(c>='A'&&c<='Z')o+=(char)c; else if(c>='a'&&c<='z')o+=(char)(c-32); }
    if(o.size()<3){
        for(char c:upperStr(iso)) if(c>='A'&&c<='Z')o+=c;
        uint32_t h=pcHash(city);
        while(o.size()<3){ o+=(char)('A'+(h%26)); h/=26; }
    }
    return o.substr(0,3);
}

struct StStyle{ int shape; float rx,ry,inner; bool arc; int rings; bool dash,ticks,stars; const char* cap; };

static StStyle stStyle(const Stamp& s,const StampCDef& d){
    uint32_t h=pcHash("stamp|"+lowerStr(s.iso));
    StStyle t{};
    static const int EU[4]={3,1,0,5};
    static const int NA[3]={2,3,1};
    static const int AS[5]={0,3,1,4,6};
    static const int SA[4]={3,0,1,7};
    static const int AF[4]={0,1,3,5};
    static const int OC[4]={0,3,1,6};
    static const int UN[5]={0,1,3,4,8};
    const int* set; int n;
    if(d.cont=="EU"){set=EU;n=4;} else if(d.cont=="NA"){set=NA;n=3;}
    else if(d.cont=="AS"){set=AS;n=5;} else if(d.cont=="SA"){set=SA;n=4;}
    else if(d.cont=="AF"){set=AF;n=4;} else if(d.cont=="OC"){set=OC;n=4;}
    else {set=UN;n=5;}
    t.shape=set[(d.tpl+(h>>3))%n];
    t.rings=1+((h>>7)%2);
    t.dash=((h>>9)&1)!=0;
    t.ticks=((h>>11)&1)!=0;
    t.stars=((h>>13)&1)!=0;
    static const char* CAP[6]={"IMMIGRATION","BORDER CONTROL","AIRWAVES CONTROL",
                               "PASSPORT CONTROL","IMMIGRACION","ENTRY CONTROL"};
    t.cap=CAP[(h>>15)%6];
    if(d.cont=="SA"&&((h>>19)&1))t.cap="MIGRACIONES";
    switch(t.shape){
        case 1: t.rx=1.00f;t.ry=0.74f;t.inner=1.00f;t.arc=true; break;
        case 2: t.rx=0.86f;t.ry=0.60f;t.inner=1.00f;t.arc=false;break;
        case 3: t.rx=0.88f;t.ry=0.66f;t.inner=1.00f;t.arc=false;break;
        case 4: t.rx=0.95f;t.ry=0.90f;t.inner=0.84f;t.arc=true; break;
        case 5: t.rx=0.95f;t.ry=0.95f;t.inner=0.90f;t.arc=true; break;
        case 6: t.rx=0.80f;t.ry=0.94f;t.inner=1.00f;t.arc=false;break;
        case 7: t.rx=0.84f;t.ry=0.92f;t.inner=1.00f;t.arc=false;break;
        case 8: t.rx=0.98f;t.ry=0.98f;t.inner=0.58f;t.arc=false;break;
        default:t.rx=1.00f;t.ry=1.00f;t.inner=1.00f;t.arc=true; break;
    }
    return t;
}

static SDL_Color stInk(const Stamp& s,const StampCDef& d){
    static const SDL_Color P[7][3]={
        {C(28,32,48),C(38,44,92),C(24,60,96)},
        {C(30,32,44),C(150,38,44),C(28,60,88)},
        {C(150,38,44),C(28,32,48),C(24,92,62)},
        {C(24,92,62),C(58,42,96),C(28,86,96)},
        {C(28,32,48),C(24,92,62),C(96,66,40)},
        {C(28,86,96),C(30,32,44),C(58,42,96)},
        {C(70,72,78),C(28,32,48),C(58,42,96)}
    };
    int r=d.cont=="EU"?0:d.cont=="NA"?1:d.cont=="AS"?2:d.cont=="SA"?3:
          d.cont=="AF"?4:d.cont=="OC"?5:6;
    return P[r][((unsigned)s.ink)%3];
}
static std::string stDate(const Stamp& s){
    const StampCDef& d=stDef(s.iso);
    tm g=tmUtc((time_t)s.ts+(time_t)s.tzMin*60);
    char b[32];
    if(d.date==1)snprintf(b,32,"%04d-%02d-%02d",g.tm_year+1900,g.tm_mon+1,g.tm_mday);
    else if(d.date==2){const char* m[]={"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};
        snprintf(b,32,"%s %02d %04d",m[g.tm_mon],g.tm_mday,g.tm_year+1900);}
    else if(d.date==3)snprintf(b,32,"%02d/%02d/%02d",g.tm_mday,g.tm_mon+1,(g.tm_year+1900)%100);
    else if(d.date==4)snprintf(b,32,"%02d.%02d.%04d",g.tm_mday,g.tm_mon+1,g.tm_year+1900);
    else if(d.date==6)snprintf(b,32,"%02d.%02d.%02d",g.tm_mday,g.tm_mon+1,(g.tm_year+1900+543)%100);
    else if(d.date==5){int y=g.tm_year+1900; if(y>=2019)snprintf(b,32,"R%02d.%02d.%02d",y-2018,g.tm_mon+1,g.tm_mday);
        else snprintf(b,32,"H%02d.%02d.%02d",y-1988,g.tm_mon+1,g.tm_mday);}
    else snprintf(b,32,"%02d.%02d.%02d",g.tm_mday,g.tm_mon+1,(g.tm_year+1900)%100);
    return b;
}
static StampDB stLoad(){
    StampDB d; std::string body;
    if(!readFile(stPath(),body))return d;
    J j(body); if(!j.eat('{'))return d;
    std::string k;
    while(objNext(j,k)){
        if(k=="stamps"){
            if(!j.eat('['))break;
            while(arrNext(j)){
                Stamp s; if(!j.eat('{'))break;
                std::string sk;
                while(objNext(j,sk)){
                    if(sk=="iso")s.iso=j.str(); else if(sk=="country")s.country=j.str();
                    else if(sk=="city")s.city=j.str(); else if(sk=="place")s.placeId=j.str();
                    else if(sk=="station")s.station=j.str(); else if(sk=="ts")s.ts=(long long)j.num();
                    else if(sk=="tz")s.tzMin=(int)j.num(); else if(sk=="type")s.type=(int)j.num();
                    else if(sk=="air")s.airSec=(int)j.num(); else if(sk=="ink")s.ink=(int)j.num();
                    else if(sk=="seed")s.seed=(uint32_t)j.num(); else if(sk=="ch")s.cityHash=(uint32_t)j.num();
                    else if(sk=="sh")s.stationHash=(uint32_t)j.num(); else j.skip();
                }
                j.eat('}'); if(!j.ok)break;
                if(!s.iso.empty()&&s.ts>0)d.v.push_back(std::move(s));
            }
            j.eat(']');
        }
        else if(k=="sec"){ if(j.eat('{')){std::string sk;while(objNext(j,sk)){d.sec[sk]=(int)j.num();}j.eat('}');} }
        else if(k=="last"){ if(j.eat('{')){std::string sk;while(objNext(j,sk)){d.last[sk]=(long long)j.num();}j.eat('}');} }
        else if(k=="done"){ if(j.eat('[')){while(arrNext(j))d.done.insert(j.str());j.eat(']');} }
        else j.skip();
        if(!j.ok)break;
    }
    if(d.v.size()>300)d.v.erase(d.v.begin(),d.v.begin()+(d.v.size()-300));
    return d;
}
static void stSave(const StampDB& d){
    std::string o="{\"stamps\":[";
    for(size_t i=0;i<d.v.size();i++){const Stamp& s=d.v[i]; if(i)o+=",";
        o+="{\"iso\":\""+jsonEsc(s.iso)+"\",\"country\":\""+jsonEsc(s.country)+"\",\"city\":\""+jsonEsc(s.city)+
        "\",\"place\":\""+jsonEsc(s.placeId)+"\",\"station\":\""+jsonEsc(s.station)+"\",\"ts\":"+std::to_string(s.ts)+
        ",\"tz\":"+std::to_string(s.tzMin)+",\"type\":"+std::to_string(s.type)+",\"air\":"+std::to_string(s.airSec)+
        ",\"ink\":"+std::to_string(s.ink)+",\"seed\":"+std::to_string(s.seed)+",\"ch\":"+std::to_string(s.cityHash)+
        ",\"sh\":"+std::to_string(s.stationHash)+"}";}
    o+="],\"sec\":{"; bool f=true;
    for(auto& kv:d.sec){if(!f)o+=",";f=false;o+="\""+jsonEsc(kv.first)+"\":"+std::to_string(kv.second);}
    o+="},\"last\":{"; f=true;
    for(auto& kv:d.last){if(!f)o+=",";f=false;o+="\""+jsonEsc(kv.first)+"\":"+std::to_string(kv.second);}
    o+="},\"done\":["; f=true;
    for(auto& s:d.done){if(!f)o+=",";f=false;o+="\""+jsonEsc(s)+"\"";}
    o+="]}";
    writeFile(stPath(),o);
}
static bool gAccountOk=false;
struct SwProfile {
    bool ok=false,isMii=false;
    std::string nick,miiJpeg;
    std::vector<uint8_t> miiRgba;
    int miiW=0,miiH=0;
    uint64_t seed=0;
};
static SwProfile fetchSwitchProfile(const AccountUid* want=nullptr){
    SwProfile r;
    if(!gAccountOk)return r;
    AccountUid uid{};
    if(want)uid=*want;
    else if(R_FAILED(accountGetLastOpenedUser(&uid))||(uid.uid[0]==0&&uid.uid[1]==0)){
        AccountUid uids[8]; s32 n=0;
        if(R_FAILED(accountListAllUsers(uids,8,&n))||n<=0)return r;
        uid=uids[0];
    }
    r.seed=uid.uid[0]^(uid.uid[1]*0x9E3779B97F4A7C15ull);
    AccountProfile prof{};
    Result rc=accountGetProfile(&prof,uid);
    if(R_FAILED(rc)){
        return r;
    }
    AccountUserData ud{}; AccountProfileBase base{};
    if(R_SUCCEEDED(accountProfileGet(&prof,&ud,&base))){
        const char* nk=(const char*)base.nickname;
        size_t n=0; while(n<0x20&&nk[n])++n;
        r.nick=trimStr(std::string(nk,n));
        r.isMii=(ud.iconID==0);
    }
    u32 sz=0;
    rc=accountProfileGetImageSize(&prof,&sz);
    if(R_SUCCEEDED(rc)&&sz>64&&sz<=0x40000){
        std::vector<uint8_t> buf(sz); u32 got=0;
        rc=accountProfileLoadImage(&prof,buf.data(),buf.size(),&got);
        if(R_SUCCEEDED(rc)&&got>64){
            r.miiJpeg.assign((const char*)buf.data(),got);
            int w=0,h=0,ch=0;
            stbi_uc* px=stbi_load_from_memory(buf.data(),(int)got,&w,&h,&ch,4);
            if(px&&w>0&&h>0){
                r.miiRgba.assign(px,px+(size_t)w*h*4);
                r.miiW=w; r.miiH=h;
            }
            if(px)stbi_image_free(px);
        }
    }
    accountProfileClose(&prof);
    r.ok=!r.nick.empty()||!r.miiRgba.empty()||!r.miiJpeg.empty();
    return r;
}
struct PcLay {
    float w=0,h=0,tx=0,nameY=0,ctryY=0,twid=0;
    SDL_Rect photo{0,0,0,0};
    float stampX=0,stampY=0,stampW=54,stampH=64;
    int nf=F30;
};
static PcLay pcLayout(const Card& c){
    PcLay L; int fmt=c.variant%3;
    if(fmt==0){ L.w=430;L.h=574; L.photo={22,22,386,392};
        L.tx=22;L.nameY=436;L.ctryY=482;L.twid=316;L.nf=F30; L.stampX=354;L.stampY=488; }
    else if(fmt==1){ L.w=620;L.h=466; L.photo={22,22,340,422};
        L.tx=384;L.nameY=64;L.ctryY=112;L.twid=212;L.nf=F24; L.stampX=544;L.stampY=380; }
    else { L.w=520;L.h=520; L.photo={22,22,476,356};
        L.tx=22;L.nameY=396;L.ctryY=442;L.twid=406;L.nf=F30; L.stampX=444;L.stampY=392; }
    return L;
}

struct App {
    SDL_Window* win=nullptr; SDL_Renderer* ren=nullptr;
    Batch b; Sky sky; Globe globe; Player player; Audio audio; State state; Fonts fonts;
    SDL_Texture* bgTex=nullptr; SDL_GameController* pad=nullptr;
    Worker worker;
    Worker pcWorker;
    std::vector<Place> places; bool placesReady=false,placesError=false,landStarted=false;
    std::set<std::string> deadPlaces,brokenCh; bool deadDirty=false;
    int screen=SCR_GLOBE; float panel=0; int libTab=LT_FAV;
    bool cleanMode=false; float cleanMix=0;
Passport pp;
Card pcCur; bool pcCurValid=false;
bool pcOpen=false; float pcMix=0;
struct PcTex { SDL_Texture* tex=nullptr; int w=0,h=0; };
PcTex pcPhoto;
Slot<PcPhotoRes> pcSlot;
SDL_Texture* pcFace=nullptr; std::string pcFaceKey; int pcFaceW=0,pcFaceH=0;
SDL_Rect pcRect{0,0,0,0};
bool passportOpen=false; float passportMix=0; bool ppChimed=false;
SDL_Texture* miiTex=nullptr; uint64_t ppSeed=1; float blinkT=3.f,blinkP=0;
SDL_Texture* coverTex=nullptr; SDL_Rect bookRect{0,0,0,0};
std::map<int,SDL_Texture*> pgTex;
std::map<int,std::string> pgKey;
int ppSpread=0,ppDir=0; float ppTurn=0; bool ppTurning=false;
StampDB stamps;
struct StPlace { int page=0,idx=0; float x=0,y=0,r=0,rot=0; };
std::vector<StPlace> stPlace;
bool stLayoutDirty=true;
int stAnimIdx=-1;
SDL_Texture* stAnimTex=nullptr; int stAnimTexIdx=-1;
std::map<std::string,SDL_Texture*> stTexCache;
std::map<std::string,Uint32> stTexUse;
int stBakeBudget=0;
std::map<int,Uint32> pgUse;
std::map<int,std::string> visaKeyBase;
std::map<int,std::vector<int>> visaIdx;
bool visaKeyDirty=true;
Uint32 labEvictAt=0;
std::set<std::string> stMapIso; bool stMapIsoDirty=true;
std::vector<std::string> placeIso;
float stAnimT=0,stShake=0,stMapUntil=0;
std::string stCur;
int stSaveAcc=0;
std::map<std::string,SDL_Texture*> stOvr;
std::set<std::string> stOvrTried;
float airAcc=0; int airSaveT=0;
std::map<std::string,Uint32> flagUse;
    Worker gtaWorker; Slot<GtaRes> gtaSlot;
    Worker gtaImgWorker;
    struct GtaPx{std::string rel;int w=0,h=0;std::vector<uint8_t> rgba;};
    std::mutex gtaPxMtx;
    std::deque<GtaPx> gtaPxQ;
    std::set<std::string> gtaPxPending;
    std::vector<GtaStation> gta;
    int gtaCnt[GTA_NG]={}; size_t gtaCntN=0;
    bool gtaMode=false,nowGta=false,gtaReady=false;
    float gtaMix=0,gtaPoll=0,gtaLastReq=-1e9f;
    int gtaView=0,gtaPick=0,gtaGame=0,gtaSel=0;
    float gtaViewMix=0,gtaCar=0;
    bool gtaCarDrag=false; float gtaCarStart=0;
    std::vector<int> gtaRows;
    struct GtaFx2{float lift=0,liftV=0,sc=.92f,scV=0,glow=0,ph=0;} gtaFx2[GTA_NG];
    bool gtaFxInit=false;
    GtaWorld gtaWorld;
    struct GtaTx2{SDL_Texture* tex=nullptr;int w=0,h=0;Uint32 use=0;};
    std::map<std::string,GtaTx2> gtaTx2;
    std::set<std::string> gtaTx2Miss;
    std::string gtaNowMount;
    float gtaGradF[4][3]={{0}}; bool gtaGradInit=false;
    float gtaAuraPulse=0, gtaAuraEnergy=0, gtaAuraSpin=0, gtaAuraTint=0, gtaAuraTintT=0;
    HomeMenu home; bool homeMode=true; float homeMix=1.f;
    bool minusDown=false,plusDown=false,comboFired=false;
    Place curCity; CityRes cityRes; bool cityLoading=false; Place cityWant; bool cityWantSet=false;
    Slot<CityRes> citySlot;
    std::string query; std::vector<SearchHit> hits; bool searchLoading=false;
    std::string searchWant; bool searchWantSet=false; Slot<std::vector<SearchHit>> searchSlot; std::string searchRunning;
    Slot<PlacesRes> placesSlot; Slot<std::vector<std::vector<Vec2>>> landSlot;
    Slot<RandomRes> randomSlot; bool randomBusy=false;
    Slot<WxRes> wxSlot; std::string wxKey; WxRes nowWx; std::map<std::string,WxRes> wxCache;
    Worker flagWorker[2]; int flagRR=0;
    std::mutex flagMtx; std::deque<FlagRes> flagQ;
    std::set<std::string> flagPending;
    Slot<GeoRes> geoSlot; bool geoAsked=false;
    std::map<std::string,SDL_Texture*> flagTex; std::set<std::string> flagTried;
    std::vector<std::pair<std::string,std::string>> histView;
    std::vector<Row> rows; std::vector<int> selRows; int selIdx=0;
    float scroll=0,rowsH=0,focusY=0,scrollTarget=0,focusTarget=0;
    struct Now { std::string chId,title,place,placeId,country,cc,url; float lat=0,lon=0;
        int tzMin=0; bool haveTz=false,valid=false; } now;
    bool failNotified=false;
    float eq[NB]={},eqT[NB]={}; float eqPeak=-70; int eqTick=0;
    std::vector<Toast> toasts; std::vector<UiHit> uiHits;
    bool clipOn=false; SDL_Rect clipR{0,0,0,0}; Uint32 frameNo=0;
    std::map<std::string,std::vector<Run>> runCache;
    std::map<std::string,int> widthCache;
    std::map<std::string,std::string> truncCache;
    std::map<std::string,Lab> labs;
    bool stateDirty=false; float saveTimer=0;
    float t=0; Uint32 last=0; bool running=true;
    bool didInit=false;
    bool focused=true;
    bool mediaOn=false;
    bool bl=false,br=false;
    float pinchDist=0; int fingers=0;
    std::map<SDL_FingerID,Vec2> touch;
    enum { TZ_NONE=0,TZ_GLOBE,TZ_LIST,TZ_HOME,TZ_PASSPORT,TZ_CARD,TZ_UI,TZ_GTA };
    SDL_FingerID tPrimary=-1;
    Vec2 tStart{0,0},tPrev{0,0},tCur{0,0};
    Uint32 tDownT=0,tLastMoveT=0;
    float tTravel=0; int tZone=TZ_NONE; bool tActive=false;
    Uint32 lastTapT=0; Vec2 lastTapP{0,0};
    bool scrollManual=false; float scrollVel=0; float listScrollMax=0;
    SDL_Rect panelListRect{0,0,0,0}; bool panelListOn=false;
    SDL_Rect nowCardRect{0,0,0,0}; bool nowCardOn=false;
    bool lblInit=false,screenOff=false; float savedBrightness=1.f;
    float idleT=0,nightMix=0; bool nightMode=false;
    float sleepLeft=0; int sleepMin=0; float osdVol=0,tuneAnim=0;
    bool greeted=false;
    std::string clockStr="00:00",localStr="00:00"; float clockTimer=0,cachedHour=12;
    void touchState(){ stateDirty=true; saveTimer=1.5f; }
    void flushState(){ if(stateDirty){ saveState(state); stateDirty=false; saveTimer=0; } }
    void flushDead(){ if(deadDirty){ saveIdSet("dead.txt",deadPlaces); saveIdSet("broken.txt",brokenCh); deadDirty=false; } }
    std::string gtaAssetPath(const std::string& rel){
        static std::map<std::string,std::string> cache;
        auto it=cache.find(rel);
        if(it!=cache.end())return it->second;
        std::string found;
        for(const std::string& path:{gAppDir+"gta/"+rel,gAppDir+"assets/gta/"+rel,gCacheDir+"gta/"+rel,std::string("romfs:/gta/")+rel}){
            if(fileExists(path)){found=path;break;}
        }
        cache.emplace(rel,found);
        return found;
    }
    SDL_Texture* gtaTex2(const std::string& rel,int maxDim){
        if(rel.empty())return nullptr;
        auto it=gtaTx2.find(rel);
        if(it!=gtaTx2.end()){it->second.use=frameNo;return it->second.tex;}
        if(gtaTx2Miss.count(rel))return nullptr;
        std::string path=gtaAssetPath(rel);
        if(path.empty()){gtaTx2Miss.insert(rel);return nullptr;}
        bool crop=rel.rfind("stations/",0)==0;
        {
            std::lock_guard<std::mutex> lk(gtaPxMtx);
            if(gtaPxPending.count(rel))return nullptr;
            gtaPxPending.insert(rel);
        }
        gtaImgWorker.post([this,path,rel,maxDim,crop]{
            GtaPx result;result.rel=rel;
            std::string body;
            if(readFile(path,body)){
                if(decodeImageFit(body,maxDim,result.rgba,result.w,result.h)&&result.w>0&&result.h>0){
                    if(crop)gtaTrimAlpha(result.rgba,result.w,result.h);
                } else {result.w=result.h=0;}
            }
            std::lock_guard<std::mutex> lk(gtaPxMtx);
            gtaPxQ.push_back(std::move(result));
        });
        return nullptr;
    }
    void gtaUploadPx(){
        int budget=3;
        while(budget>0){
            GtaPx result;
            {
                std::lock_guard<std::mutex> lk(gtaPxMtx);
                if(gtaPxQ.empty())return;
                result=std::move(gtaPxQ.front());
                gtaPxQ.pop_front();
                gtaPxPending.erase(result.rel);
            }
            budget--;
            if(result.w<=0||result.h<=0){gtaTx2Miss.insert(result.rel);continue;}
            GtaTx2 texture;
            texture.tex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,result.w,result.h);
            if(!texture.tex){gtaTx2Miss.insert(result.rel);continue;}
            SDL_UpdateTexture(texture.tex,nullptr,result.rgba.data(),result.w*4);
            SDL_SetTextureBlendMode(texture.tex,SDL_BLENDMODE_BLEND);
            texture.w=result.w;texture.h=result.h;texture.use=frameNo;
            gtaTx2[result.rel]=texture;
            if(gtaTx2.size()>44){
                std::string old;Uint32 best=0xFFFFFFFFu;
                for(auto& kv:gtaTx2)if(kv.second.use!=frameNo&&kv.second.use<best){best=kv.second.use;old=kv.first;}
                if(!old.empty()){
                    auto d=gtaTx2.find(old);
                    if(d!=gtaTx2.end()){if(d->second.tex)SDL_DestroyTexture(d->second.tex);gtaTx2.erase(d);}
                }
            }
        }
    }
    void gtaFreeTx2(){
        for(auto& kv:gtaTx2)if(kv.second.tex)SDL_DestroyTexture(kv.second.tex);
        gtaTx2.clear();gtaTx2Miss.clear();
        std::lock_guard<std::mutex> lk(gtaPxMtx);
        gtaPxQ.clear();gtaPxPending.clear();
    }
    void gtaLogo(const std::string& rel,const SDL_Rect& box,int maxDim,int a,int bright){
        SDL_Texture* tx=gtaTex2(rel,maxDim);if(!tx||a<=0||box.w<=0||box.h<=0)return;
        int iw=0,ih=0;SDL_QueryTexture(tx,nullptr,nullptr,&iw,&ih);
        if(iw<=0||ih<=0)return;
        float scale=std::min(box.w/(float)iw,box.h/(float)ih);
        int dw=std::max(1,(int)(iw*scale+.5f)),dh=std::max(1,(int)(ih*scale+.5f));
        SDL_Rect dst{box.x+(box.w-dw)/2,box.y+(box.h-dh)/2,dw,dh};
        Uint8 mod=(Uint8)clampf((float)bright,0.f,255.f);
        SDL_SetTextureColorMod(tx,mod,mod,mod);
        SDL_SetTextureAlphaMod(tx,(Uint8)clampf((float)a,0.f,255.f));
        SDL_RenderCopy(ren,tx,nullptr,&dst);
        SDL_SetTextureColorMod(tx,255,255,255);SDL_SetTextureAlphaMod(tx,255);
    }
    void gtaCoverFill(const std::string& rel,const SDL_Rect& box,int maxDim,int a,int bright){
        SDL_Texture* texture=gtaTex2(rel,maxDim);
        if(!texture||a<=0||box.w<=0||box.h<=0)return;
        int iw=0,ih=0;SDL_QueryTexture(texture,nullptr,nullptr,&iw,&ih);
        if(iw<=0||ih<=0)return;
        float target=box.w/(float)box.h,source=iw/(float)ih;
        SDL_Rect src;
        if(source>target){src.h=ih;src.w=(int)(ih*target+.5f);src.x=(iw-src.w)/2;src.y=0;}
        else{src.w=iw;src.h=(int)(iw/target+.5f);src.x=0;src.y=(ih-src.h)/2;}
        Uint8 mod=(Uint8)clampf((float)bright,0.f,255.f);
        SDL_SetTextureColorMod(texture,mod,mod,mod);
        SDL_SetTextureAlphaMod(texture,(Uint8)clampf((float)a,0.f,255.f));
        SDL_RenderCopy(ren,texture,&src,&box);
        SDL_SetTextureColorMod(texture,255,255,255);
        SDL_SetTextureAlphaMod(texture,255);
    }
    std::string gtaCover2(const std::string& key){
        if(!gtaAssetPath("games/"+key+".png").empty())return "games/"+key+".png";
        if(!gtaAssetPath("games/"+key+"_background.png").empty())return "games/"+key+"_background.png";
        return "";
    }
    std::string gtaGLogo2(const std::string& key){
        const char* suffix[]={"_logo.png","_logo-mobile.png","_logo_gta_logo-1.png","_logo-animated.png"};
        for(const char* tail:suffix){std::string rel="games/"+key+tail;if(!gtaAssetPath(rel).empty())return rel;}
        return "";
    }
    int gtaWrap(int index){int n=(int)gtaRows.size();if(n<=0)return 0;index%=n;if(index<0)index+=n;return index;}
    void gtaWarmText(int row){
        if(row<0||row>=(int)gtaRows.size())return;
        const GtaStation& station=gta[gtaRows[row]];
        lab(F30,trunc(F30,station.name,780,true),Pal::text,true);
        lab(F17,trunc(F17,station.genre,780),Pal::dim,false);
    }
    void gtaRebuild(){
        for(int i=0;i<GTA_NG;i++)gtaCnt[i]=0;
        for(auto& st:gta){int gi=gtaGameIdx(st.game);if(gi<GTA_NG)gtaCnt[gi]++;}
        gtaCntN=gta.size();
        if(gtaGame<0||gtaGame>=GTA_NG)gtaGame=0;
        gtaRows.clear();
        for(size_t i=0;i<gta.size();i++)if(gtaGameIdx(gta[i].game)==gtaGame)gtaRows.push_back((int)i);
        gtaSel=std::max(0,std::min(gtaSel,std::max(0,(int)gtaRows.size()-1)));
    }
    void gtaInit(){
        gGtaSfx.load(0,"sfx/change-station-sa.mp3",2.5f);
        gGtaSfx.load(1,"sfx/change-station-vc.mp3",2.5f);
        std::map<std::string,std::vector<GtaStation>> catalog;
        if(gtaLoadCatalog(catalog)){
            for(int i=0;i<GTA_NG;i++){
                auto it=catalog.find(GTA_GAMES[i].key);
                if(it==catalog.end())continue;
                for(auto& station:it->second)gta.push_back(station);
            }
            gtaReady=!gta.empty();
        } else {
            std::string body;
            if(readFile(gCacheDir+"gta_live.json",body)){
                GtaRes result=parseGtaStatus(body);
                if(result.ok){
                    for(auto& station:result.v)if(gtaGameIdx(station.game)<GTA_NG)gta.push_back(station);
                    gtaReady=!gta.empty();
                }
            }
        }
        gtaRebuild();
        for(int i=0;i<GTA_NG;i++){
            std::string cover=gtaCover2(GTA_GAMES[i].key);
            if(!cover.empty())gtaTex2(cover,384);
            std::string logo=gtaGLogo2(GTA_GAMES[i].key);
            if(!logo.empty())gtaTex2(logo,256);
        }
        for(auto& v:GTA_V){
            GtaStation st; st.mount=std::string("5/")+v.slug; st.game="5";
            st.name=v.name; st.genre=v.genre; st.url="";
            gta.push_back(st);
        }
        for(auto& v:GTA_VI){
            GtaStation st; st.mount=std::string("6/")+v.slug; st.game="6";
            st.name=v.name; st.genre=v.genre; st.url="";
            gta.push_back(st);
        }
        gtaWorker.post([]{ iaLoad(); if(!gIaReady||!iaHasVI()) iaFetch(); });
        gtaReady=true;gtaRebuild();
    }
    void gtaRequest(){
        if(t-gtaLastReq<20.f)return;
        if(gtaSlot.busy.exchange(true))return;
        gtaLastReq=t;
        gtaWorker.post([this]{gtaSlot.put(fetchGtaLive());});
    }
    void gtaSetSel(int row){
        int count=(int)gtaRows.size();if(count<=0)return;
        int next=gtaWrap(row);if(next==gtaSel)return;
        gtaSel=next;gtaWarmText(next);audio.sfx.trigger(Sfx::TICK);
        gtaAuraPulse=std::max(gtaAuraPulse,.40f);
    }
    void gtaMove(int delta){gtaSetSel(gtaSel+delta);}
    void gtaEnterGame(int gi){
        if(gi<0||gi>=GTA_NG)return;
        if(gtaCnt[gi]<=0){
            if(!gtaReady){gtaRequest();toast("GTA Radio catalog is loading",Pal::dim);}
            else toast(std::string(GTA_GAMES[gi].label)+" · Coming soon",Pal::warm);
            audio.sfx.trigger(Sfx::BACK);return;
        }
        gtaGame=gi;gtaPick=gi;gtaRows.clear();
        for(size_t i=0;i<gta.size();i++)if(gtaGameIdx(gta[i].game)==gtaGame)gtaRows.push_back((int)i);
        gtaSel=0;gtaCar=0;gtaCarDrag=false;gtaView=1;
        for(int index:gtaRows)gtaTex2(gtaLogoRel(gta[index].mount),256);
        gtaWarmText(0);
        audio.sfx.trigger(Sfx::POP);
    }
    void gtaBackToShelf(){
        if(gtaView==0)return;
        gtaView=0;gtaCarDrag=false;audio.sfx.trigger(Sfx::BACK);
    }
    void gtaNextGame(int delta){
        for(int k=1;k<=GTA_NG;k++){
            int gi=(gtaGame+delta*k+GTA_NG*GTA_NG)%GTA_NG;
            if(gtaCnt[gi]>0){gtaEnterGame(gi);return;}
        }
    }
    void gtaMoveCard(int dx,int dy){
        int d=dy?dy:dx; if(!d)return;
        int next=(gtaPick+d+GTA_NG)%GTA_NG;
        if(next!=gtaPick){gtaPick=next;audio.sfx.trigger(Sfx::TICK);}
    }
    void enterGta(){
        if(gtaMode)return;
        if(cleanMode)setClean(false);
        gtaMode=true;
        if(!gIaBusy)gtaWorker.post([]{ iaLoad(); if(!gIaReady||!iaHasVI()) iaFetch(); });
        homeMode=false;home.latch=false;globe.home=true;globe.flying=false;
        screen=SCR_GLOBE;rebuildRows(false);
        gtaView=0;gtaViewMix=0;gtaPick=gtaGame;gtaCarDrag=false;
        gtaPoll=0;if(!gtaReady)gtaRequest();
        audio.sfx.trigger(Sfx::POP);
    }
    void exitGta(){
        if(!gtaMode)return;
        gtaMode=false;gtaCarDrag=false;audio.sfx.trigger(Sfx::BACK);
    }
    void gtaPlayCore(const std::string& id,const std::string& title,const std::string& url,const std::string& place){
        std::string mount=id.rfind("gta:",0)==0?id.substr(4):"";
        std::string game=mount.substr(0,mount.find('/'));
        const bool isV=(game=="5"||game=="6");
        std::string stream=url;
        if(!isV&&stream.empty()&&!mount.empty())stream=std::string(GTA_AUDIO)+"/"+mount;
        if(!isV&&stream.empty()){toast("Stream URL missing",Pal::danger);return;}

        std::vector<std::string> c; std::vector<int64_t> offs,secs; int64_t dur=0;
        if(isV){
            if(!gtaVCandidates(mount,c,offs,secs,dur)){toast("Station unavailable",Pal::danger);return;}
            stream=c[0];
        }else{
            c.push_back(stream);
            c.push_back(std::string("http://185.221.215.185:8000/")+mount);
            if(stream.rfind("https://",0)==0)c.push_back("http://"+stream.substr(8));
        }

        now=Now{id,title,place,"","GTA Radio","",stream,0.f,0.f,gtaCityTzMin(gtaGameIdx(game)),true,true};
        nowGta=true;gtaNowMount=mount;
        failNotified=false;tuneAnim=0;nowWx=WxRes{};stLeave();
        globe.setSelected("");globe.setSelPoint(0,0,false);refreshClocks();
        if(state.uiSounds){
            if(game=="sa")gGtaSfx.trigger(0);
            else if(game=="vc"||game=="vcs")gGtaSfx.trigger(1);
            else audio.sfx.trigger(Sfx::SWEEP);
        }
        player.liveOn=isV;
        player.liveDur=isV?dur:0;
        player.livePhase=isV?gtaVPhase(mount):0;
        player.startList(c,offs,secs);
        gtaAuraPulse=1.f;
        gtaAuraTintT=(float)(pcHash(mount)%1000)/1000.f;
        if(screen==SCR_LIB&&libTab==LT_HIST)histView=player.hist();
    }
    void gtaPlay(int row){
        if(row<0||row>=(int)gtaRows.size())return;
        const GtaStation& st=gta[gtaRows[row]];gtaPlayCore("gta:"+st.mount,st.name,st.url,gtaGameLabel(st.game));
    }
    void gtaRandom(){
        if(gtaRows.empty())return;
        int row=(int)randIndex(gtaRows.size());gtaSetSel(row);gtaPlay(row);
    }
    void gtaFav(){
        if(nowGta&&now.valid){Channel ch{now.chId,now.title,now.url,"",now.place,"GTA Radio","",0,0};toggleFav(ch);return;}
        if(gtaSel<0||gtaSel>=(int)gtaRows.size())return;
        const GtaStation& st=gta[gtaRows[gtaSel]];
        Channel ch{"gta:"+st.mount,st.name,st.url,"",gtaGameLabel(st.game),"GTA Radio","",0,0};toggleFav(ch);
    }
    void gtaUpdate(float dt){
        gtaUploadPx();
        GtaRes res;
        if(gtaSlot.take(res)&&res.ok){
            std::string selectedMount;
            if(gtaView==1&&gtaSel>=0&&gtaSel<(int)gtaRows.size()){
                int index=gtaRows[gtaSel];
                if(index>=0&&index<(int)gta.size())selectedMount=gta[index].mount;
            }
            bool added=false;
            for(auto& live:res.v){
                if(gtaGameIdx(live.game)>=GTA_NG)continue;
                bool found=false;
                for(auto& station:gta)if(station.mount==live.mount){
                    station.title=live.title;station.listeners=live.listeners;station.bitrate=live.bitrate;found=true;break;
                }
                if(!found){gta.push_back(live);added=true;}
            }
            gtaReady=true;
            if(added){
                gtaRebuild();
                if(gtaView==1&&!selectedMount.empty())for(size_t i=0;i<gtaRows.size();i++){
                    if(gta[gtaRows[i]].mount==selectedMount){gtaSel=(int)i;break;}
                }
            }
        }
        if(!(gtaMode||nowGta))return;
        if(!focused||screenOff)return;
        gtaPoll-=dt;
        if(gtaPoll<=0&&!gtaSlot.busy.load()){
            gtaPoll=gtaMode?60.f:180.f;
            gtaRequest();
        }
    }
    void gtaCarUpdate(float dt){
        if(gtaCntN!=gta.size()){
            gtaCntN=gta.size();
            for(int i=0;i<GTA_NG;i++)gtaCnt[i]=0;
            for(auto& st:gta){int gi=gtaGameIdx(st.game);if(gi<GTA_NG)gtaCnt[gi]++;}
        }
        if(!gtaFxInit){gtaFxInit=true;for(int i=0;i<GTA_NG;i++)gtaFx2[i].ph=i*1.37f;}
        gtaMix=lerpf(gtaMix,gtaMode?1.f:0.f,1-powf(.004f,dt));
        if(gtaMode&&gtaMix>.995f)gtaMix=1.f;
        if(!gtaMode&&gtaMix<.005f)gtaMix=0.f;
        gtaViewMix=lerpf(gtaViewMix,gtaView?1.f:0.f,1-powf(.002f,dt));
        if(gtaView&&gtaViewMix>.995f)gtaViewMix=1.f;
        if(!gtaView&&gtaViewMix<.005f)gtaViewMix=0.f;
        for(int i=0;i<GTA_NG;i++){
            GtaFx2& f=gtaFx2[i];
            bool selected=(gtaMode&&gtaView==0&&i==gtaPick);
            HomeMenu::spring(f.lift,f.liftV,selected?-13.f:0.f,170.f,2e-5f,dt);
            HomeMenu::spring(f.sc,f.scV,selected?1.06f:.92f,150.f,4e-5f,dt);
            f.glow=lerpf(f.glow,selected?1.f:0.f,1-powf(.0025f,dt));
        }
        gtaGradUpdate(dt);
        gtaAuraUpdate(dt);
        gtaWorld.update(dt,gtaMix,easeOut(clampf(gtaViewMix,0.f,1.f)),
                        gtaView==1?gtaGame:gtaPick,gtaMode,globe);
        int n=(int)gtaRows.size();
        if(n<=0){gtaCar=0;return;}
        while(gtaCar<0)gtaCar+=n;
        while(gtaCar>=n)gtaCar-=n;
        if(!gtaCarDrag){
            float delta=(float)gtaSel-gtaCar;
            while(delta>n*.5f)delta-=n;
            while(delta<-n*.5f)delta+=n;
            gtaCar+=delta*(1-powf(.00002f,dt));
            if(fabsf(delta)<.002f)gtaCar=(float)gtaSel;
            while(gtaCar<0)gtaCar+=n;
            while(gtaCar>=n)gtaCar-=n;
        }
    }

    void gtaGradSnap(){
        int gi=gtaView==1?gtaGame:gtaPick;
        if(gi<0||gi>=GTA_NG)gi=0;
        const GtaGradDef& d=GTA_GRAD[gi];
        for(int i=0;i<4;i++){
            SDL_Color c=hsv(d.h[i],d.s[i],d.v[i]);
            gtaGradF[i][0]=(float)c.r;gtaGradF[i][1]=(float)c.g;gtaGradF[i][2]=(float)c.b;
        }
    }
    void gtaGradUpdate(float dt){
        if(!gtaGradInit){gtaGradInit=true;gtaGradSnap();}
        int gi=gtaView==1?gtaGame:gtaPick;
        if(gi<0||gi>=GTA_NG)gi=0;
        const GtaGradDef& d=GTA_GRAD[gi];
        float k=1-powf(.045f,dt);
        for(int i=0;i<4;i++){
            SDL_Color tgt=hsv(d.h[i],d.s[i],d.v[i]);
            gtaGradF[i][0]=lerpf(gtaGradF[i][0],(float)tgt.r,k);
            gtaGradF[i][1]=lerpf(gtaGradF[i][1],(float)tgt.g,k);
            gtaGradF[i][2]=lerpf(gtaGradF[i][2],(float)tgt.b,k);
        }
    }
    SDL_Color gtaAuraCol(float u,float a){
        float f=fmodf(u,1.f); if(f<0)f+=1.f; f*=4.f;
        int i0=((int)f)&3, i1=(i0+1)&3;
        float t=f-floorf(f); t=t*t*(3.f-2.f*t);
        return C((int)lerpf(gtaGradF[i0][0],gtaGradF[i1][0],t),
                 (int)lerpf(gtaGradF[i0][1],gtaGradF[i1][1],t),
                 (int)lerpf(gtaGradF[i0][2],gtaGradF[i1][2],t),
                 (int)clampf(a,0.f,255.f));
    }

    void gtaAuraUpdate(float dt){
        float e=0;
        for(int i=0;i<NB;i++) e+=eq[i]*(i<12?1.3f:.65f);   // низы весят больше
        e=clampf(e/(float)NB*2.1f,0.f,1.f);
        float k=1-powf(e>gtaAuraEnergy?.0004f:.015f,dt);   // быстрая атака, мягкий спад
        gtaAuraEnergy=lerpf(gtaAuraEnergy,e,k);
        gtaAuraPulse=std::max(0.f,gtaAuraPulse-dt*1.5f);
        gtaAuraSpin+=dt*(.055f+.22f*gtaAuraEnergy);        // переливание крутится от бита
        gtaAuraTint=lerpf(gtaAuraTint,gtaAuraTintT,1-powf(.02f,dt));
    }

    void gtaAura(float amount){
        if(amount<.01f)return;
        bool on=nowGta&&player.state==Player::PLAYING;
        float live=on?1.f:0.f, en=gtaAuraEnergy, pu=gtaAuraPulse;
        float A=amount*(.30f+.70f*live)*(.60f+.40f*en)+amount*pu*.5f;
        if(A<.012f)return;
        const float cx=globe.center.x, cy=globe.center.y, R=std::max(1.f,globe.R);
        const int NS=90;
        float spin=gtaAuraSpin+gtaAuraTint;
        auto seg=[&](float r0,float r1,float u0,float u1,float a0,float a1,float t0,float t1){
            float c0=cosf(t0),s0=sinf(t0),c1=cosf(t1),s1=sinf(t1);
            int p0=b.vert(cx+c0*r0,cy+s0*r0,gtaAuraCol(u0,a0));
            int p1=b.vert(cx+c1*r0,cy+s1*r0,gtaAuraCol(u1,a0));
            int p2=b.vert(cx+c1*r1,cy+s1*r1,gtaAuraCol(u1,a1));
            int p3=b.vert(cx+c0*r1,cy+s0*r1,gtaAuraCol(u0,a1));
            b.tri(p0,p1,p2); b.tri(p0,p2,p3);
        };
        for(int k=0;k<3;k++){                              // мягкое гало наружу
            float r0=R*(1.004f+.010f*k);
            float r1=R*(1.045f+.070f*k+.055f*en+.070f*pu);
            float a0=A*(46.f-12.f*k);
            for(int i=0;i<NS;i++){
                float t0=2*(float)M_PI*i/NS, t1=2*(float)M_PI*(i+1)/NS;
                seg(r0,r1,(float)i/NS+spin,(float)(i+1)/NS+spin,a0,0.f,t0,t1);
            }
        }
        b.flush(ren);
        {                                                  // яркая кромка
            float r0=R, r1=R*(1.014f+.006f*en), a0=A*(120.f+90.f*en);
            for(int i=0;i<NS;i++){
                float t0=2*(float)M_PI*i/NS, t1=2*(float)M_PI*(i+1)/NS;
                seg(r0,r1,(float)i/NS+spin,(float)(i+1)/NS+spin,a0,a0*.55f,t0,t1);
            }
            b.flush(ren);
        }
        if(live>.01f){                                     // радиальный эквалайзер
            const int BARS=72;
            for(int i=0;i<BARS;i++){
                float u=(float)i/BARS;
                float m=fabsf(u*2.f-1.f);                  // зеркальный спектр
                int bi=(int)(m*(NB-1)); if(bi<0)bi=0; if(bi>NB-1)bi=NB-1;
                float v=eq[bi]*live; if(v<.02f)continue;
                float t0=u*2*(float)M_PI-(float)M_PI*.5f;
                float half=(2*(float)M_PI/BARS)*.34f;
                float r0=R*1.016f, r1=r0+R*(.010f+.085f*v);
                float a=A*(60.f+165.f*v);
                seg(r0,r1,u+spin,u+spin,a,a*.15f,t0-half,t0+half);
            }
            b.flush(ren);
        }
        if(pu>.01f){                                       // вспышка при переключении
            float p=1.f-pu;
            float r0=R*(1.01f+.20f*p), r1=r0+R*.035f*(1.f-p);
            float a=amount*pu*pu*150.f;
            for(int i=0;i<NS;i++){
                float t0=2*(float)M_PI*i/NS, t1=2*(float)M_PI*(i+1)/NS;
                seg(r0,r1,(float)i/NS+spin,(float)(i+1)/NS+spin,a,0.f,t0,t1);
            }
            b.flush(ren);
        }
    }

    void gtaMap(float amount,float view){
        float a=amount*lerpf(1.f,.82f,view);
        if(a<.02f)return;
        int gi=gtaView==1?gtaGame:gtaPick;
        SDL_Color acc=hsv(GTA_GAMES[gi].hue,.45f,.98f);
        gtaWorld.drawCities(ren,b,globe,t,a,gtaWorld.dot,acc);
        for(int i=0;i<3;i++){
            Vec2 s; if(!gtaWorld.cityScreen(globe,i,s))continue;
            bool on=(i==gtaWorld.dot);
            int al=(int)(a*(on?240:125));
            txt(on?F17:F15,GTA_USA[i].name,(int)(s.x+18),(int)(s.y-(on?10:8)),
                alpha(on?Pal::text:Pal::dim,al),on);
        }
    }
    void gtaPanelHead(float amount,float view){
        const float RX=796.f,RW=448.f;
        int A=(int)(235*amount);
        if(view<.5f){
            txt(F24,"GTA RADIO",(int)RX,26,alpha(Pal::text,A),true);
        } else {
            std::string lg=gtaGLogo2(GTA_GAMES[gtaGame].key);
            if(!lg.empty())gtaLogo(lg,SDL_Rect{(int)RX,20,220,44},256,A,255);
            else txt(F24,upperStr(gtaGameLabel(GTA_GAMES[gtaGame].key)),(int)RX,26,alpha(Pal::text,A),true);
        }
        b.line(RX,84.f,RX+RW,84.f,1.f,alpha(Pal::wire,(int)(16*amount)));
        b.flush(ren);
    }
    void gtaWorlds(float amount){
        if(amount<.01f)return;
        float vw=easeOut(clampf(gtaViewMix,0.f,1.f));
        const float RX=796.f,RW=448.f,RH=72.f,STEP=78.f,Y0=98.f;
        for(int i=0;i<GTA_NG;i++){
            const GtaGameDef& game=GTA_GAMES[i]; GtaFx2& fx=gtaFx2[i];
            bool avail=gtaCnt[i]>0;
            float st=easeOut(clampf((gtaWorld.enterT-.05f*i)/.5f,0.f,1.f));
            float a=amount*st; if(a<.02f)continue;
            int A=(int)(255*a);
            float x=RX+fx.lift*.7f+(1.f-st)*26.f;
            float y=Y0+i*STEP-vw*18.f;
            SDL_Color own=hsv(game.hue,.55f,.95f);
            if(fx.glow>.01f){
                b.rrect2(x,y,RW,RH,10.f,8.f,alpha(Pal::wire,(int)(11*fx.glow*a)));
                b.flush(ren);
                b.rrect2(x,y+10.f,3.f,RH-20.f,1.5f,1.5f,alpha(own,(int)(230*fx.glow*a)));
                b.flush(ren);
            }
            int bright=(int)(lerpf(96.f,255.f,fx.glow)*(avail?1.f:.45f));
            std::string lg=gtaGLogo2(game.key);
            float ls=lerpf(.94f,1.f,fx.glow);
            int lw=(int)(210*ls),lh=(int)(52*ls);
            if(!lg.empty())gtaLogo(lg,SDL_Rect{(int)(x+20),(int)(y+RH*.5f-lh*.5f),lw,lh},256,A,bright);
            else txt(F20,upperStr(game.label),(int)(x+20),(int)(y+RH*.5f-12),alpha(Pal::text,A),true);
            GtaCityDef c=gtaCity(i);
            int tx=(int)(x+248);
            bool bold=fx.glow>.5f;
            txt(F20,trunc(F20,c.city,(int)(RW-268),bold),tx,(int)(y+RH*.5f-12),
                alpha(mixc(Pal::dim,Pal::text,fx.glow),A),bold);
            if(vw<.5f)hit(H_GTA_GAME,(int)RX,(int)y,(int)RW,(int)RH,i);
        }
    }
    void gtaStations(float amount){
        if(amount<.01f)return;
        float vw=easeOut(clampf(gtaViewMix,0.f,1.f));
        int n=(int)gtaRows.size();
        const float CX=1020.f,CY=350.f,SP=170.f,BW=232.f,BH=200.f;
        float slide=(1.f-vw)*18.f;
        if(n<=0){
            Icon::spinner(b,CX,CY+slide,18,t*4,alpha(Pal::accent,(int)(200*amount)));
            b.flush(ren);
            return;
        }
        SDL_Rect clip{782,230,474,246};
        SDL_RenderSetClipRect(ren,&clip);
        clipOn=true; clipR=clip;
        for(int pass=0;pass<2;pass++){
            for(int i=0;i<n;i++){
                float d=(float)i-gtaCar;
                while(d>n*.5f)d-=n;
                while(d<-n*.5f)d+=n;
                float dist=fabsf(d); if(dist>2.3f)continue;
                bool center=dist<=.5f; if(pass==0?center:!center)continue;
                float sg=d<0?-1.f:1.f;
                float k=dist<=1.f?dist:1.f+(dist-1.f)*.42f;
                float x=CX+sg*k*SP;
                float sc=dist<=1.f?lerpf(1.f,.48f,dist):.48f;
                float far=clampf(dist-1.f,0.f,1.3f)/1.3f;
                float op=dist<=1.f?lerpf(1.f,.34f,dist):lerpf(.34f,.12f,far);
                int br=(int)(dist<=1.f?lerpf(255.f,104.f,dist):lerpf(104.f,64.f,far));
                int w=(int)(BW*sc),h=(int)(BH*sc);
                SDL_Rect box{(int)(x-w*.5f),(int)(CY+slide-h*.5f),w,h};
                gtaLogo(gtaLogoRel(gta[gtaRows[i]].mount),box,256,(int)(255*op*amount),br);
                if(vw>.5f)hit(H_GTA_ROW,box.x,box.y,box.w,box.h,i);
            }
        }
        SDL_RenderSetClipRect(ren,nullptr); clipOn=false;
        float fr=fabsf(gtaCar-floorf(gtaCar+.5f));
        int ta=(int)(255*clampf(1.f-fr*2.f,0.f,1.f)*amount);
        if(ta>2&&gtaSel>=0&&gtaSel<n){
            const GtaStation& s=gta[gtaRows[gtaSel]];
            txt(F24,trunc(F24,s.name,452,true),(int)CX,(int)(476+slide),alpha(Pal::text,ta),true,1);
            txt(F15,trunc(F15,s.genre,452),(int)CX,(int)(510+slide),alpha(Pal::dim,(int)(ta*.85f)),false,1);
            bool live=nowGta&&gtaNowMount==s.mount&&player.state==Player::PLAYING;
            float pl=.5f+.5f*sinf(t*3.f);
            b.circle(CX,554.f+slide,3.6f,alpha(live?Pal::live:Pal::faint,(int)(ta*(live?.6f+.4f*pl:.45f))),8);
            b.flush(ren);
        }
    }
    void drawGta(){
        float amount=easeOut(gtaMix); if(amount<.004f)return;
        float view=easeOut(clampf(gtaViewMix,0.f,1.f));
        SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
        int dim=(int)(112.f*view*amount);
        if(dim>0){
            SDL_SetRenderDrawColor(ren,0,0,0,(Uint8)dim);
            SDL_Rect full{0,0,W,H}; SDL_RenderFillRect(ren,&full);
        }
        gtaAura(amount);
        gtaMap(amount,view);
        gtaPanelHead(amount,view);
        if(view<.995f)gtaWorlds(amount*(1.f-view));
        if(view>.005f)gtaStations(amount*view);
    }

    void setClean(bool on){
        if(cleanMode==on)return;
        if(on&&gtaMode)exitGta();
        cleanMode=on; globe.zen=on;
        if(on){
            screen=SCR_GLOBE;
            rebuildRows(false);
            nightMode=false;
            globe.targetR=Globe::DEF_R;
            toast("Clean mode · hold − and + to exit",Pal::accentSoft);
        } else toast("Clean mode off",Pal::dim);
        audio.sfx.trigger(Sfx::POP);
    }
    void toggleClean(){ setClean(!cleanMode); }
    void cleanUpdate(float dt){
        cleanMix=lerpf(cleanMix,cleanMode?1.f:0.f,1-powf(.002f,dt));
        if(cleanMode){
            globe.targetR=Globe::DEF_R;
            if(screen!=SCR_GLOBE){ screen=SCR_GLOBE; rebuildRows(false); }
        }
    }
    static std::string keyOf(int fi,bool bold,const std::string& s){
        std::string k; k.reserve(s.size()+4); k+=(char)('0'+fi); k+=(bold?'1':'0'); k+=s; return k;
    }
    const std::vector<Run>& runs(int fi,const std::string& s){
        std::string key=keyOf(fi,false,s);
        auto it=runCache.find(key);
        if(it!=runCache.end())return it->second;
        if(runCache.size()>4000)runCache.clear();
        std::vector<Run> out; size_t i=0,start=0; int cur=-1;
        while(i<s.size()){
            size_t prev=i; unsigned cp=utf8Next(s,i); int fam=fonts.famFor(cp,fi);
            if(fam!=cur){ if(cur>=0)out.push_back({cur,s.substr(start,prev-start)}); cur=fam; start=prev; }
        }
        if(cur>=0)out.push_back({cur,s.substr(start)});
        return runCache.emplace(key,std::move(out)).first->second;
    }
    int tw(int fi,const std::string& s,bool bold=false){
        if(s.empty())return 0;
        std::string k=keyOf(fi,bold,s);
        auto it=widthCache.find(k);
        if(it!=widthCache.end())return it->second;
        int total=0;
        for(auto& r:runs(fi,s)){
            TTF_Font* f=fonts.get(fi,r.fam); if(!f)continue;
            int w=0,h=0;
            TTF_SetFontStyle(f,bold?TTF_STYLE_BOLD:TTF_STYLE_NORMAL);
            TTF_SizeUTF8(f,r.text.c_str(),&w,&h); total+=w;
        }
        if(widthCache.size()>6000)widthCache.clear();
        widthCache.emplace(k,total);
        return total;
    }
    std::string trunc(int fi,const std::string& s,int maxW,bool bold=false){
        if(s.empty()||maxW<=0)return s;
        std::string k=keyOf(fi,bold,s); k+="|"; k+=std::to_string(maxW);
        auto it=truncCache.find(k);
        if(it!=truncCache.end())return it->second;
        std::string res=s;
        if(tw(fi,s,bold)>maxW){
            std::vector<size_t> bd{0};
            for(size_t i=0;i<s.size();){ utf8Next(s,i); bd.push_back(i); }
            size_t lo=0,hi=bd.size()-1,best=0;
            while(lo<=hi){
                size_t mid=(lo+hi)/2;
                if(tw(fi,s.substr(0,bd[mid])+"…",bold)<=maxW){ best=mid; lo=mid+1; }
                else { if(!mid)break; hi=mid-1; }
            }
            res=s.substr(0,bd[best])+"…";
        }
        if(truncCache.size()>4000)truncCache.clear();
        truncCache.emplace(k,res);
        return res;
    }
    void evictLabs(){
        if(labs.size()<=1500)return;
        std::vector<std::pair<Uint32,std::string>> v;
        v.reserve(labs.size());
        for(auto& kv:labs)if(kv.second.use!=frameNo)v.push_back({kv.second.use,kv.first});
        if(v.empty())return;
        size_t n=std::min<size_t>(400,v.size());
        std::partial_sort(v.begin(),v.begin()+n,v.end());
        for(size_t i=0;i<n;i++){
            auto it=labs.find(v[i].second);
            if(it==labs.end())continue;
            for(auto& p:it->second.p)if(p.tex)SDL_DestroyTexture(p.tex);
            labs.erase(it);
        }
    }
    Lab& lab(int fi,const std::string& s,SDL_Color c,bool bold){
        std::string key=keyOf(fi,bold,s);key+="|";
        key+=std::to_string((int)c.r)+","+std::to_string((int)c.g)+","+std::to_string((int)c.b);
        auto it=labs.find(key);
        if(it==labs.end()){
            if(labs.size()>1500&&frameNo-labEvictAt>30){labEvictAt=frameNo;evictLabs();}
            Lab created;
            for(auto& run:runs(fi,s)){
                TTF_Font* font=fonts.get(fi,run.fam);
                if(!font)continue;
                TTF_SetFontStyle(font,bold?TTF_STYLE_BOLD:TTF_STYLE_NORMAL);
                SDL_Color solid=c;solid.a=255;
                SDL_Surface* surface=TTF_RenderUTF8_Blended(font,run.text.c_str(),solid);
                if(!surface)continue;
                Piece piece;piece.tex=SDL_CreateTextureFromSurface(ren,surface);piece.w=surface->w;piece.h=surface->h;
                SDL_FreeSurface(surface);
                if(!piece.tex)continue;
                created.w+=piece.w;
                if(piece.h>created.h)created.h=piece.h;
                created.p.push_back(piece);
            }
            it=labs.emplace(key,std::move(created)).first;
        }
        it->second.use=frameNo;
        return it->second;
    }
    void txt(int fi,const std::string& s,int x,int y,SDL_Color c,bool bold=false,int align=0){
        if(s.empty()||c.a==0)return;
        Lab& cached=lab(fi,s,c,bold);
        int cx=align==1?x-cached.w/2:(align==2?x-cached.w:x);
        for(auto& piece:cached.p){
            if(!piece.tex)continue;
            SDL_Rect dst{cx,y,piece.w,piece.h};
            SDL_SetTextureAlphaMod(piece.tex,c.a);
            SDL_RenderCopy(ren,piece.tex,nullptr,&dst);
            cx+=piece.w;
        }
    }
    void marquee(int fi,const std::string& s,int x,int y,int maxW,SDL_Color c,
                 bool bold=false,float speed=44.f){
        if(s.empty()||maxW<=8||c.a==0)return;
        int full=tw(fi,s,bold);
        if(full<=maxW){ txt(fi,s,x,y,c,bold); return; }
        const float gap=56.f, hold=1.15f;
        float L=(float)full+gap, T=L/speed+hold;
        float ph=fmodf(t,T);
        float off=(ph<hold)?0.f:(ph-hold)*speed;
        SDL_Rect old; SDL_RenderGetClipRect(ren,&old);
        SDL_Rect clip{x,y-2,maxW,(int)(FONT_PX[fi]*1.7f)+2};
        SDL_RenderSetClipRect(ren,&clip);
        int ix=x-(int)off;
        txt(fi,s,ix,y,c,bold);
        if(off>L-(float)maxW)txt(fi,s,ix+(int)L,y,c,bold);
        if(old.w>0&&old.h>0)SDL_RenderSetClipRect(ren,&old);
        else SDL_RenderSetClipRect(ren,nullptr);
    }
    void clearLabs(){
        for(auto& kv:labs)for(auto& p:kv.second.p)if(p.tex)SDL_DestroyTexture(p.tex);
        labs.clear();
        for(auto& kv:flagTex)if(kv.second)SDL_DestroyTexture(kv.second);
        flagTex.clear();
    }
    void plate(float x,float y,float w,float h,float r,SDL_Color body){
        b.rrect2(x-2,y-2,w+4,h+4,r+2,r+1,alpha(Pal::wire,5));
        b.rrect2(x,y,w,h,r,r*.72f,body);
        b.line(x+r*.7f,y+1,x+w-r*.7f,y+1,1,alpha(Pal::wire,16));
    }
    void buildBackground(){
        std::vector<Uint32> px((size_t)W*H,0xFF000000u);
        for(int i=0;i<240;i++){
            int x=(int)frand(0,(float)W),y=(int)frand(0,(float)H),a=(int)frand(8,26);
            Uint32 d=px[(size_t)y*W+x];
            int r=(d>>16)&255,g=(d>>8)&255,bb=d&255;
            r+=(255-r)*a/255; g+=(255-g)*a/255; bb+=(255-bb)*a/255;
            px[(size_t)y*W+x]=0xFF000000u|((Uint32)r<<16)|((Uint32)g<<8)|(Uint32)bb;
        }
        bgTex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_STATIC,W,H);
        if(bgTex){ SDL_UpdateTexture(bgTex,nullptr,px.data(),W*4); SDL_SetTextureBlendMode(bgTex,SDL_BLENDMODE_NONE); }
    }
    void requestWx(float lat,float lon){
        if(lat==0&&lon==0)return;
        char kb[40]; snprintf(kb,40,"%.2f_%.2f",lat,lon);
        std::string key=kb;
        auto it=wxCache.find(key);
        if(it!=wxCache.end()){ nowWx=it->second; return; }
        if(wxKey==key&&wxSlot.busy.load())return;
        wxKey=key; nowWx=WxRes{};
        if(wxSlot.busy.exchange(true))return;
        worker.post([this,lat,lon,key]{
            WxRes r; std::string e; char url[256];
            snprintf(url,256,"https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f&current=temperature_2m,weather_code&timezone=auto",lat,lon);
            std::string body=httpGet(url,e,14,false);
            if(!body.empty()){
                J j(body);
                if(j.eat('{')){
                    std::string k;
                    while(objNext(j,k)){
                        if(k=="utc_offset_seconds")r.tz=(int)j.num();
                        else if(k=="current"){
                            if(j.eat('{')){ std::string ck;
                                while(objNext(j,ck)){
                                    if(ck=="temperature_2m"){ r.temp=(float)j.num(); r.ok=true; }
                                    else if(ck=="weather_code")r.code=(int)j.num();
                                    else j.skip(); }
                                j.eat('}'); }
                        } else j.skip();
                    }
                }
            }
            r.key=key;
            wxSlot.put(std::move(r));
        });
    }
    void requestFlag(const std::string& iso){
        if(iso.empty())return;
        std::string low=lowerStr(iso);
        if(flagTex.count(low)||flagTried.count(low))return;
        {
            std::lock_guard<std::mutex> lk(flagMtx);
            if(flagPending.count(low))return;
            flagPending.insert(low);
        }
        Worker& w=flagWorker[(flagRR++)&1];
        w.post([this,low]{
            FlagRes r; r.iso=low;
            std::string path=gCacheDir+"flags/"+low+".png",body;
            if(!readFile(path,body)){
                std::string e; body=httpGet("https://flagcdn.com/w160/"+low+".png",e,12,false,false);
                if(body.size()>8){ makeDirs(gCacheDir+"flags"); writeFile(path,body); }
            }
            if(body.size()>8)r.ok=decodePNG(body,r.w,r.h,r.rgba);
            std::lock_guard<std::mutex> lk(flagMtx);
            flagQ.push_back(std::move(r));
        });
    }
    void ppPrefetchFlags(){
        for(auto& ck:pp.countries){
            if(ck.empty()||ck.rfind("n:",0)==0)continue;
            std::string low=lowerStr(ck);
            if(flagTex.count(low))continue;
            std::string body;
            if(readFile(gCacheDir+"flags/"+low+".png",body)&&body.size()>8){
                FlagRes r; r.iso=low;
                if(decodePNG(body,r.w,r.h,r.rgba)&&r.w>0&&r.h>0){
                    gFlagPal[low]=pcPalFromPixels(r.rgba,r.w,r.h);
                    SDL_Texture* tx=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,r.w,r.h);
                    if(tx){
                        SDL_UpdateTexture(tx,nullptr,r.rgba.data(),r.w*4);
                        SDL_SetTextureBlendMode(tx,SDL_BLENDMODE_BLEND);
                        flagTex[low]=tx; flagUse[low]=frameNo;
                    }
                }
                flagTried.insert(low);
            } else requestFlag(low);
        }
    }

void pollAsync(){
    WxRes wr; if(wxSlot.take(wr)){ if(!wr.key.empty())wxCache[wr.key]=wr; if(wr.key==wxKey)nowWx=wr; }
    for(;;){
        FlagRes fr; bool have=false;
        {
            std::lock_guard<std::mutex> lk(flagMtx);
            if(!flagQ.empty()){
                fr=std::move(flagQ.front());flagQ.pop_front();
                flagPending.erase(fr.iso);
                have=true;
            }
        }
        if(!have)break;
        flagTried.insert(fr.iso);
        if(fr.ok&&fr.w>0&&fr.h>0){
            gFlagPal[fr.iso]=pcPalFromPixels(fr.rgba,fr.w,fr.h);
            SDL_Texture* tx=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,fr.w,fr.h);
            if(tx){
                SDL_UpdateTexture(tx,nullptr,fr.rgba.data(),fr.w*4);
                SDL_SetTextureBlendMode(tx,SDL_BLENDMODE_BLEND);
                flagTex[fr.iso]=tx; flagUse[fr.iso]=frameNo;
            }
        }
    }
    if(flagTex.size()>128){
        std::string old; Uint32 best=0xFFFFFFFFu;
        for(auto& kv:flagUse)if(flagTex.count(kv.first)&&kv.second<best){best=kv.second;old=kv.first;}
        if(!old.empty()){
            auto d=flagTex.find(old);
            if(d!=flagTex.end()){SDL_DestroyTexture(d->second);flagTex.erase(d);}
            flagUse.erase(old); flagTried.erase(old);
        }
    }
    GeoRes gr;
    if(geoSlot.take(gr))if(gr.ok)globe.flyTo(gr.lat,gr.lon);
}
    void refreshClocks(){
        tm l=tmLocal(time(nullptr)); char c[8];
        snprintf(c,8,"%02d:%02d",l.tm_hour,l.tm_min); clockStr=c;
        int off=now.haveTz?now.tzMin*60:(nowWx.ok?nowWx.tz:(int)lroundf(now.lon/15.f)*3600);
        tm g=tmUtc(time(nullptr)+(time_t)off);
        snprintf(c,8,"%02d:%02d",g.tm_hour,g.tm_min); localStr=c;
        cachedHour=now.valid?(g.tm_hour+g.tm_min/60.f):12.f;
    }
    void toast(const std::string& s,SDL_Color c=Pal::text){
        toasts.push_back({s,0,c});
        if(toasts.size()>3)toasts.erase(toasts.begin());
    }
    void hit(int id,int x,int y,int w,int h,int data=0){
        SDL_Rect r{x,y,w,h};
        if(clipOn){
            int x0=std::max(r.x,clipR.x),y0=std::max(r.y,clipR.y);
            int x1=std::min(r.x+r.w,clipR.x+clipR.w),y1=std::min(r.y+r.h,clipR.y+clipR.h);
            if(x1<=x0||y1<=y0)return;
            r=SDL_Rect{x0,y0,x1-x0,y1-y0};
        }
        uiHits.push_back({id,r,data});
    }
    int hitAt(int x,int y){
        for(int i=(int)uiHits.size()-1;i>=0;i--){ SDL_Rect& r=uiHits[i].r;
            if(x>=r.x&&x<r.x+r.w&&y>=r.y&&y<r.y+r.h)return uiHits[i].id; }
        return H_NONE;
    }
    int hitData(int x,int y){
        for(int i=(int)uiHits.size()-1;i>=0;i--){ SDL_Rect& r=uiHits[i].r;
            if(x>=r.x&&x<r.x+r.w&&y>=r.y&&y<r.y+r.h)return uiHits[i].data; }
        return 0;
    }
    int touchZone(float x,float y){
        if(gtaMode&&gtaMix>.4f)return TZ_GTA;
        if(pcOpen&&pcMix>.4f)return TZ_CARD;
        if(passportOpen&&passportMix>.4f)return TZ_PASSPORT;
        if(homeMode)return TZ_HOME;
        if(nowCardOn&&x>=nowCardRect.x&&x<nowCardRect.x+nowCardRect.w&&
           y>=nowCardRect.y&&y<nowCardRect.y+nowCardRect.h)return TZ_UI;
        if(panel>.5f){
            if(panelListOn&&x>=panelListRect.x&&x<panelListRect.x+panelListRect.w&&
               y>=panelListRect.y&&y<panelListRect.y+panelListRect.h)return TZ_LIST;
            if(x>=panelX())return TZ_UI;
        }
        return TZ_GLOBE;
    }
    int touchPick(float x,float y){
        int best=-1; float bd=44.f*44.f;
        for(size_t i=0;i<globe.pp.size();i++){
            if(globe.pz[i]<=0)continue;
            float dx=globe.pp[i].x-x,dy=globe.pp[i].y-y,d=dx*dx+dy*dy;
            if(d<bd){bd=d;best=(int)i;}
        }
        return best;
    }
    void touchHit(int id,int data){
        switch(id){
        case H_PLAYPAUSE: player.togglePause(); break;
        case H_FAVNOW: if(now.valid){Channel c{now.chId,now.title,now.url,now.placeId,now.place,now.country,now.cc,now.lat,now.lon}; toggleFav(c);} break;
        case H_PLACE: { const Place* p=placeById(now.placeId); if(p)openCity(*p); } break;
        case H_BACK: pressBack(); break;
        case H_ROW: if(data>=0&&data<(int)selRows.size()){ selIdx=data; scrollManual=false; activate(); } break;
        case H_FIELD: openInput(); break;
        case H_RETRY: placesError=false; startPlaces(); break;
        case H_TAB: if(screen==SCR_LIB&&data>=0&&data<4){ libTab=data; if(libTab==LT_HIST)histView=player.hist(); audio.sfx.trigger(Sfx::TICK); rebuildRows(false);} break;
        case H_HOME: if(homeMode){ if(data==home.sel)homeActivate(); else { while(home.sel!=data)home.move(data>home.sel?1:-1); audio.sfx.trigger(Sfx::TICK);} } break;
        case H_CARD: pcShow(curCity); break;
        case H_PC_CLOSE: pcClose(); break;
        case H_PC_RETRY: pcRetry(); break;
        }
    }
    void touchTap(float x,float y){
        wake();
        if(nightMix>.5f)return;
        int id=hitAt((int)x,(int)y),data=hitData((int)x,(int)y);
        if(id==H_GTA_ROW){
            if(gtaView==1&&!gtaRows.empty()&&data>=0&&data<(int)gtaRows.size()){
                if(data==gtaSel){
                    const GtaStation& station=gta[gtaRows[gtaSel]];
                    if(nowGta&&gtaNowMount==station.mount)player.togglePause();
                    else gtaPlay(gtaSel);
                } else gtaSetSel(data);
            }
            return;
        }
        if(id==H_GTA_GAME){gtaPick=data;gtaEnterGame(data);return;}
        if(id!=H_NONE){ touchHit(id,data); lastTapT=SDL_GetTicks(); lastTapP={x,y}; return; }
        if(tZone==TZ_PASSPORT){
            if(y<70.f||x<bookRect.x||x>bookRect.x+bookRect.w||y>bookRect.y+bookRect.h){ closePassport(); return; }
            ppTurnPage(x<640.f?-1:1);
            return;
        }
        if(tZone==TZ_CARD){
            if(x<pcRect.x||x>pcRect.x+pcRect.w||y<pcRect.y||y>pcRect.y+pcRect.h)pcClose();
            return;
        }
        if(tZone==TZ_GLOBE&&!homeMode&&placesReady){
            int idx=touchPick(x,y);
            if(idx>=0&&(size_t)idx<places.size()){ openCity(places[idx]); return; }
            Uint32 now=SDL_GetTicks();
            if(now-lastTapT<330&&fabsf(x-lastTapP.x)+fabsf(y-lastTapP.y)<48.f&&!cleanMode){
                globe.zoomIdle=0; globe.flying=false;
                globe.targetR=(globe.targetR>380.f)?190.f:Globe::DEF_R;
                lastTapT=0;
            } else { lastTapT=now; lastTapP={x,y}; }
        }
    }
    void touchBegin(float x,float y,SDL_FingerID id){
        tPrimary=id; tActive=true;
        tStart=tPrev=tCur=Vec2{x,y};
        tTravel=0; tDownT=SDL_GetTicks(); tLastMoveT=tDownT;
        tZone=touchZone(x,y);
        if(tZone==TZ_LIST)scrollVel=0;
    }
    void touchMove(float x,float y){
        if(!tActive)return;
        float dx=x-tPrev.x,dy=y-tPrev.y;
        tCur={x,y}; tTravel+=fabsf(dx)+fabsf(dy); tPrev={x,y};
        if(tTravel<10.f)return;
        idleT=0;
        if(tZone==TZ_GLOBE){
            if(!homeMode&&!pcOpen&&!passportOpen)globe.drag(dx,dy);
        } else if(tZone==TZ_LIST){
            scrollManual=true;
            scroll=clampf(scroll-dy,0.f,listScrollMax);
            Uint32 now=SDL_GetTicks();
            float edt=std::max(1.f,(float)(now-tLastMoveT))/1000.f;
            tLastMoveT=now;
            scrollVel=clampf(lerpf(scrollVel,-dy/edt,.55f),-2600.f,2600.f);
        } else if(tZone==TZ_GTA&&gtaView==1){
            int count=(int)gtaRows.size();
            if(count>0){
                if(!gtaCarDrag){gtaCarDrag=true;gtaCarStart=gtaCar;}
                gtaCar-=dx/158.f;
                while(gtaCar<0)gtaCar+=count;
                while(gtaCar>=count)gtaCar-=count;
            }
        }
    }
    void touchEnd(float x,float y){
        if(!tActive)return;
        tActive=false;
        Uint32 now=SDL_GetTicks();
        float dx=x-tStart.x,dy=y-tStart.y;
        if(tTravel<12.f&&now-tDownT<450){ touchTap(x,y); return; }
        if(tZone==TZ_PASSPORT){
            if(fabsf(dx)>60.f&&fabsf(dx)>fabsf(dy)*1.4f)ppTurnPage(dx<0?1:-1);
            else if(dy>90.f&&fabsf(dy)>fabsf(dx)*1.4f)closePassport();
        } else if(tZone==TZ_HOME){
            if(fabsf(dx)>50.f&&fabsf(dx)>fabsf(dy)){ home.move(dx<0?1:-1); audio.sfx.trigger(Sfx::TICK); }
        } else if(tZone==TZ_CARD){
            if(dy>80.f&&fabsf(dy)>fabsf(dx))pcClose();
        } else if(tZone==TZ_LIST){
            if(now-tLastMoveT>90)scrollVel=0;
        } else if(tZone==TZ_GTA){
            if(gtaView==1){
                if(dy>90.f&&fabsf(dy)>fabsf(dx)*1.4f){gtaBackToShelf();}
                else{
                    int count=(int)gtaRows.size();
                    if(count>0){
                        float move=gtaCar-gtaCarStart;
                        while(move>count*.5f)move-=count;
                        while(move<-count*.5f)move+=count;
                        int step=(int)lroundf(move);
                        if(step==0&&fabsf(dx)>50.f&&fabsf(dx)>fabsf(dy))step=(dx<0?1:-1);
                        gtaSel=gtaWrap((int)lroundf(gtaCarStart)+step);
                    }
                }
                gtaCarDrag=false;
            } else {
                if(fabsf(dx)>50.f&&fabsf(dx)>fabsf(dy)*1.3f)gtaMoveCard(dx<0?1:-1,0);
                else if(fabsf(dy)>60.f&&fabsf(dy)>fabsf(dx)*1.3f)gtaMoveCard(0,dy<0?1:-1);
            }
        }
    }
    float panelX(){ return (float)(W-24-PW)+(1-easeOut(panel))*(PW+80); }
    float panelH(){ return now.valid?312.f:(float)(H-84-24); }
    bool isFav(const std::string& id){ for(auto& f:state.favs)if(f.chId==id)return true; return false; }
    void syncSel(){
        if(!now.valid){ globe.setSelected(""); globe.setSelPoint(0,0,false); return; }
        float la=now.lat,lo=now.lon;
        if(la==0.f&&lo==0.f){ const Place* p=placeById(now.placeId); if(p){la=p->lat;lo=p->lon;} }
        globe.setSelected("");
        globe.setSelPoint(la,lo,(la!=0.f||lo!=0.f));
    }
    void setScreenOff(bool on){
        if(!lblInit||screenOff==on)return;
        screenOff=on;
        if(on){
            lblGetCurrentBrightnessSetting(&savedBrightness);
            lblSetCurrentBrightnessSetting(0.f);
            toast("Screen off · audio keeps playing",Pal::dim);
        } else lblSetCurrentBrightnessSetting(savedBrightness);
    }
    void wake(){ if(screenOff)setScreenOff(false); idleT=0; if(nightMode){ nightMode=false; audio.sfx.trigger(Sfx::TICK); } }
    void openLib(int tab){
        if(cleanMode)setClean(false);
        screen=SCR_LIB; libTab=tab;
        if(tab==LT_HIST)histView=player.hist();
        rebuildRows(false); audio.sfx.trigger(Sfx::POP);
    }
    void openSearch(){ if(cleanMode)setClean(false); screen=SCR_SEARCH; rebuildRows(false); openInput(); }
    void cycleSleep(){
        const int o[4]={0,15,30,60}; int i=0;
        for(int k=0;k<4;k++)if(o[k]==sleepMin)i=k;
        i=(i+1)%4; sleepMin=o[i]; sleepLeft=sleepMin*60.f;
        audio.sfx.trigger(Sfx::POP);
        toast(sleepMin?("Sleep timer: "+std::to_string(sleepMin)+" min"):"Sleep timer off",Pal::warm);
    }
    void addHeader(const std::string& s){ Row r; r.kind=0; r.title=s; r.h=36; r.y=rowsH; rowsH+=r.h; rows.push_back(std::move(r)); }
    void addStation(const Channel& c,const std::string& sub){
        Row r; r.kind=1; r.title=c.title; r.sub=sub; r.ch=c;
        r.place=c.place; r.country=c.country; r.placeId=c.placeId; r.lat=c.lat; r.lon=c.lon;
        r.h=76; r.y=rowsH; rowsH+=r.h; r.sel=(int)selRows.size(); selRows.push_back((int)rows.size());
        rows.push_back(std::move(r));
    }
    void addPlaceRow(const Place& p){
        Row r; r.kind=2; r.title=p.title; r.sub=p.country; r.placeId=p.id;
        r.h=76; r.y=rowsH; rowsH+=r.h; r.sel=(int)selRows.size(); selRows.push_back((int)rows.size());
        rows.push_back(std::move(r));
    }
    void addInfo(const std::string& ti,const std::string& su){
        Row r; r.kind=5; r.title=ti; r.sub=su; r.h=64; r.y=rowsH; rowsH+=r.h;
        r.sel=(int)selRows.size(); selRows.push_back((int)rows.size()); rows.push_back(std::move(r));
    }
    void addSetting(const std::string& ti,const std::string& v,int id){
        Row r; r.kind=3; r.title=ti; r.sub=v; r.setting=id; r.h=64; r.y=rowsH; rowsH+=r.h;
        r.sel=(int)selRows.size(); selRows.push_back((int)rows.size()); rows.push_back(std::move(r));
    }
    void addPreset(int i){
        const Preset& p=state.presets[i];
        Row r; r.kind=4; r.slot=i; r.h=64; r.y=rowsH; rowsH+=r.h;
        r.title=p.set?p.title:("Preset "+std::to_string(i+1));
        r.sub=p.set?p.place:"empty · favorite button assigns";
        r.sel=(int)selRows.size(); selRows.push_back((int)rows.size()); rows.push_back(std::move(r));
    }
    const Place* placeById(const std::string& id) const {
        for(auto& p:places)if(p.id==id)return &p;
        return nullptr;
    }
    void rebuildRows(bool keepPos){
        int oldSel=selIdx; float oldScroll=scroll;
        rows.clear(); selRows.clear(); rowsH=0; selIdx=0; scroll=0; focusY=0;
        scrollManual=false; scrollVel=0;
        if(screen==SCR_CITY){
            if(!cityRes.ch.empty()){
                Row pc; pc.kind=6; pc.title="City postcard";
pc.sub="Open a keepsake of this city";
                pc.placeId=curCity.id; pc.h=64; pc.y=rowsH; rowsH+=pc.h;
                pc.sel=(int)selRows.size(); selRows.push_back((int)rows.size()); rows.push_back(std::move(pc));
                addHeader(std::to_string(cityRes.ch.size())+" STATIONS · "+upperStr(curCity.title));
                for(auto& c:cityRes.ch)if(!brokenCh.count(c.id))addStation(c,c.country);
            }
        }
        else if(screen==SCR_SEARCH){
            if(!hits.empty())addHeader("RESULTS");
            for(auto& h:hits){
                if(h.isPlace){
                    const Place* pp=placeById(h.id);
                    if(pp)addPlaceRow(*pp);
                    else { Place p; p.id=h.id; p.title=h.title;
                        p.country=h.country.empty()?h.sub:h.country;
                        p.size=h.count>0?h.count:1; addPlaceRow(p); }
                } else {
                    Channel c; c.id=h.id; c.title=h.title; c.url=rgStream(h.id);
                    c.place=h.place; c.country=h.country; c.cc=h.cc; c.placeId=h.placeId;
                    const Place* pp=placeById(h.placeId);
                    if(pp){ c.lat=pp->lat; c.lon=pp->lon;
                        if(c.place.empty())c.place=pp->title;
                        if(c.country.empty())c.country=pp->country; }
                    addStation(c,h.sub);
                }
            }
        }
        else if(screen==SCR_LIB){
            if(libTab==LT_FAV){
                if(!state.favs.empty())addHeader("SAVED STATIONS");
                for(auto& f:state.favs){
                    if(brokenCh.count(f.chId))continue;
                    Channel c{f.chId,f.title,f.url,f.placeId,f.place,f.country,f.cc,f.lat,f.lon};
                    addStation(c,f.place);
                }
            }
            else if(libTab==LT_HIST){
                if(!histView.empty())addHeader("RECENTLY PLAYED");
                for(auto& h:histView)addInfo(h.first,h.second);
            }
            else if(libTab==LT_PRESET){ addHeader("PRESETS"); for(int i=0;i<6;i++)addPreset(i); }
            else {
                addHeader("SOUND & COMFORT");
                addSetting("Interface sounds",state.uiSounds?"On":"Off",0);
                addSetting("Pause hiss",state.pauseHiss?"On":"Off",1);
                addSetting("Auto night mode",state.autoNight?"On":"Off",2);
                addSetting("Sleep timer",sleepMin?(std::to_string(sleepMin)+" min"):"Off",3);
                char vb[8]; snprintf(vb,8,"%d%%",(int)lroundf(state.volume*100));
                addSetting("Volume",vb,4);
                addSetting("Mute",audio.sfx.muted.load()?"On":"Off",5);
                addHeader("CONTROLS");
                addInfo("A select · B back","X favorite · Y random");
                addInfo("− library · + search","hold − and + together: clean mode");
                addInfo("ZL/ZR volume · L3 pause","D-pad left/right: tabs");
            }
        }
        if(keepPos&&!selRows.empty()){
            selIdx=std::max(0,std::min(oldSel,(int)selRows.size()-1));
            scroll=std::min(oldScroll,std::max(0.f,rowsH-100.f));
            focusY=rows[selRows[selIdx]].y;
        }
        if(screen==SCR_CITY&&!keepPos&&selRows.size()>1)selIdx=1;
    }
    void startPlaces(){
        if(placesSlot.busy.exchange(true))return;
        worker.post([this]{ placesSlot.put(fetchPlaces()); });
    }
    void init(){
    logLine("init: begin");
    sky.init(); fonts.init();
    state=loadState();
    loadIdSet("dead.txt",deadPlaces);
    loadIdSet("broken.txt",brokenCh);
    player.volume=state.volume;
    audio.sfx.uiOn=state.uiSounds; audio.sfx.hissOn=state.pauseHiss;
    globe.places=&places; globe.buildGrid(); globe.initCache(ren);
    buildBackground(); gtaWorld.init(ren); audio.open(&player);
    lblInit=R_SUCCEEDED(lblInitialize());
    logLine("init: services");

    worker.start();
    pcWorker.start();
    gtaWorker.start();
    gtaImgWorker.start();
    gtaInit();
    flagWorker[0].start(); flagWorker[1].start();
    logLine("init: gta");

    home.init();
    globe.home=true;
    globe.center=Vec2{Globe::HOME_CX,Globe::HOME_CY};
    globe.R=Globe::HOME_R*1.10f; globe.targetR=Globe::HOME_R;
    globe.pitch=.38f;
    startPlaces();
    refreshClocks();

    {
        // профиль определяется автоматически: тот, через который запущено приложение
        bool hadSaved=accLoad();
        if(gAccountOk){
            AccountUid cur{};
            bool have=false;
            if(R_SUCCEEDED(accountGetLastOpenedUser(&cur))&&(cur.uid[0]||cur.uid[1]))have=true;
            if(!have){
                AccountUid uids[8]; s32 n=0;
                if(R_SUCCEEDED(accountListAllUsers(uids,8,&n))&&n>0){cur=uids[0];have=true;}
            }
            if(have){gAccUid=cur;gAccUidSet=true;}
        }
        if(!hadSaved)accAdoptLegacy();   // старые passport.json/stamps.json -> первому аккаунту, один раз
        accSave();
    }
    logLine("init: account");

    pp=ppLoad(); ppEnsure(); ppSeed=pcHash(pp.nick);
    stamps=stLoad();
    {
        std::set<std::string> seen; std::vector<Stamp> keep;
        for(auto& s:stamps.v){
            if(s.type!=ST_ENTRY)continue;
            if(seen.count(s.iso))continue;
            seen.insert(s.iso); keep.push_back(s);
        }
        stamps.v.swap(keep);
        stamps.done.clear();
        for(auto& s:stamps.v){ stamps.done.insert("E:"+s.iso); stamps.last[s.iso]=s.ts; }
        stSave(stamps);
    }
    stRebuildLayout();
    logLine("init: passport");

    SwProfile sp=fetchSwitchProfile(gAccUidSet?&gAccUid:nullptr);
    if(sp.ok){
        if(!sp.nick.empty())pp.nick=sp.nick;
        ppSeed=sp.seed?sp.seed:pcHash(pp.nick);
        char nb[16]; snprintf(nb,16,"NX-%04u-%04u",(unsigned)(sp.seed%10000),(unsigned)((sp.seed>>17)%10000));
        pp.number=nb;
        if(!sp.miiRgba.empty()&&sp.miiW>0&&sp.miiH>0){
            if(miiTex){SDL_DestroyTexture(miiTex);miiTex=nullptr;}
            miiTex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,sp.miiW,sp.miiH);
            if(miiTex){
                SDL_UpdateTexture(miiTex,nullptr,sp.miiRgba.data(),sp.miiW*4);
                SDL_SetTextureBlendMode(miiTex,SDL_BLENDMODE_BLEND);
            }
        } else if(!sp.miiJpeg.empty()){
            std::vector<uint8_t> rgba; int w=0,h=0;
            if(decodeImageStb(sp.miiJpeg,256,rgba,w,h)&&w>0){
                if(miiTex){SDL_DestroyTexture(miiTex);miiTex=nullptr;}
                miiTex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,w,h);
                if(miiTex){
                    SDL_UpdateTexture(miiTex,nullptr,rgba.data(),w*4);
                    SDL_SetTextureBlendMode(miiTex,SDL_BLENDMODE_BLEND);
                }
            }
        }
        ppSave(pp);
    }
    logLine("init: profile");

    ppPrefetchFlags();
    buildCover();
    stBakeBudget=6;
    ensurePage(0);
    ensurePage(1);
    if(stPageCount()>2)ensurePage(2);
    stBakeBudget=0;
    didInit=true;
    logLine("init: done");
}
    void updateEQ(float dt){
        if(player.state==Player::PLAYING){
            static std::vector<int16_t> buf;
            player.snapshot(buf);
            static float re[1024],im[1024];
            for(int i=0;i<1024;i++){
                re[i]=buf[i]/32768.f*(.5f*(1.f-cosf(2*(float)M_PI*i/1023.f)));
                im[i]=0.f;
            }
            fft(re,im,1024);
            float db[NB],peak=-200.f;
            for(int k=0;k<NB;k++){
                float f0=42.f*powf(260.f,k/(float)NB);
                float f1=42.f*powf(260.f,(k+1)/(float)NB);
                // монитор 24 кГц — корректная привязка бинов
                int i0=std::max(1,(int)(f0*1024.f/24000.f));
                int i1=std::min(511,(int)(f1*1024.f/24000.f));
                if(i1<i0)i1=i0;
                float sum=0.f; int cnt=0;
                for(int i=i0;i<=i1;i++){ sum+=re[i]*re[i]+im[i]*im[i]; cnt++; }
                float d=20.f*log10f(sqrtf(sum/std::max(1,cnt))+1e-9f)
                       +3.f*log2f(sqrtf(f0*f1)/120.f);
                db[k]=d; if(d>peak)peak=d;
            }
            // пик: мгновенная атака, быстрое падение — полосы не зажаты
            if(peak>eqPeak)eqPeak=peak;
            else eqPeak=std::max(peak,eqPeak-30.f*dt);
            float fl=eqPeak-42.f;
            for(int k=0;k<NB;k++)
                eqT[k]=powf(clampf((db[k]-fl)/42.f,0.f,1.f),1.35f);
        } else {
            for(auto& v:eqT)v=0.f;
            eqPeak=lerpf(eqPeak,-70.f,1.f-expf(-dt/.25f));
        }
        // атака ~12 мс — мгновенный прыжок вверх, спад ~70 мс — заметное падение
        float ka=1.f-expf(-dt/.012f);
        float kr=1.f-expf(-dt/.070f);
        for(int k=0;k<NB;k++)
            eq[k]=lerpf(eq[k],eqT[k],eqT[k]>eq[k]?ka:kr);
    }
    void killPlace(const std::string& id){
        if(id.empty()||deadPlaces.count(id))return;
        deadPlaces.insert(id); deadDirty=true;
        places.erase(std::remove_if(places.begin(),places.end(),
            [&](const Place& p){return p.id==id;}),places.end());
        globe.buildPlaces();
        placeIso.clear();
        syncSel();
    }
    void pollJobs(){
        PlacesRes pr;
        if(placesSlot.take(pr)){
            if(!pr.v.empty()){
                places.clear(); places.reserve(pr.v.size());
                for(auto& p:pr.v)if(!deadPlaces.count(p.id))places.push_back(std::move(p));
                placesReady=true; placesError=false;
                state.placesVer=pr.version; touchState();
                globe.buildPlaces();
                placeIso.clear();
                syncSel();
                if(!geoAsked){ geoAsked=true; worker.post([this]{ geoSlot.put(fetchGeo()); }); }
                if(!greeted){
                    greeted=true;
                    tm lt=tmLocal(time(nullptr));
                    char db[32]; snprintf(db,sizeof db,"%04d-%02d-%02d",lt.tm_year+1900,lt.tm_mon+1,lt.tm_mday);
                    std::string today=db;
                    if(state.lastDay!=today){
                        state.lastDay=today; touchState();
                        int hh=lt.tm_hour;
                        const char* g=hh<5?"Still up":(hh<12?"Good morning":(hh<18?"Good afternoon":"Good evening"));
                        toast(std::string(g)+" · the world is on air",Pal::warmSoft);
                    } else toast(pr.cache?"Map loaded from cache":"Rotate the globe · select a city",Pal::accentSoft);
                }
            } else { placesError=true; toast("Map failed: "+pr.err,Pal::danger); }
            if(!landStarted){ landStarted=true; worker.post([this]{ std::string e; landSlot.put(fetchLand(e)); }); }
        }
        std::vector<std::vector<Vec2>> land;
        if(landSlot.take(land))globe.buildLand(land);
        CityRes cr;
        if(citySlot.take(cr)){
            if(cr.placeId==curCity.id){
                cityLoading=false;
                if(!cr.ch.empty()){ cityRes=std::move(cr); rebuildRows(false); }
                else if(cr.ok){
                    killPlace(cr.placeId); cityRes=CityRes{};
                    toast("No stations here — city removed",Pal::dim);
                    screen=SCR_GLOBE; rebuildRows(false);
                } else {
                    cityRes=CityRes{}; cityRes.err=cr.err; rebuildRows(false);
                    toast("Couldn't load stations: "+cr.err,Pal::danger);
                }
            }
            if(cityWantSet&&cityWant.id!=curCity.id){ cityWantSet=false; openCity(cityWant); }
            else cityWantSet=false;
        }
        std::vector<SearchHit> hs;
        if(searchSlot.take(hs)){
            if(searchRunning==query){ hits=std::move(hs); searchLoading=false; rebuildRows(false); }
            if(searchWantSet&&searchWant!=query){ searchWantSet=false; doSearch(); }
            else searchWantSet=false;
        }
        RandomRes rr;
        if(randomSlot.take(rr)){
            randomBusy=false;
            if(rr.ch.url.empty())toast("Random wave failed: "+rr.err,Pal::danger);
            else { play(rr.ch); globe.flyTo(rr.ch.lat,rr.ch.lon); toast("Random wave: "+rr.ch.title,Pal::warm); }
        }
        if(player.state==Player::FAILED&&now.valid&&!failNotified){
            failNotified=true;
            brokenCh.insert(now.chId); deadDirty=true;
            toast("Stream unavailable: "+player.error(),Pal::danger);
            if(screen==SCR_CITY&&cityRes.ok&&!cityRes.ch.empty()){
                bool any=false;
                for(auto& c:cityRes.ch)if(!brokenCh.count(c.id)){any=true;break;}
                if(!any){ killPlace(now.placeId); screen=SCR_GLOBE; rebuildRows(false); }
                else rebuildRows(true);
            }
        }
    }
    void openCity(const Place& p){
        if(cleanMode)return;
        if(citySlot.busy.load()){ cityWant=p; cityWantSet=true; }
        curCity=p; cityLoading=true; cityRes=CityRes{}; screen=SCR_CITY;
        rebuildRows(false);
        globe.flyTo(p.lat,p.lon);
        audio.sfx.trigger(Sfx::POP);
        if(citySlot.busy.exchange(true))return;
        Place cp=p;
        worker.post([this,cp]{ citySlot.put(fetchCity(cp)); },true);
    }
    void play(const Channel& c){
        if(c.id.rfind("gta:",0)==0){gtaPlayCore(c.id,c.title,c.url,c.place);return;}
        nowGta=false;
        player.liveOn=false; player.liveDur=0;
        now=Now{c.id,c.title,c.place,c.placeId,c.country,c.cc,
                c.url.empty()?rgStream(c.id):c.url,c.lat,c.lon,0,false,true};
        if(cityRes.placeId==c.placeId&&cityRes.haveOffset){ now.tzMin=cityRes.utcOffset; now.haveTz=true; }
        failNotified=false; tuneAnim=0;
        nowWx=WxRes{}; requestWx(c.lat,c.lon);
        requestFlag(c.cc.empty()?isoForCountry(c.country):c.cc);
        syncSel();
        refreshClocks();
        {
ppRecord(c);
stOnPlay(c);
        }
        player.start(now.url);
        if(screen==SCR_LIB&&libTab==LT_HIST)histView=player.hist();
    }
    void toggleFav(const Channel& c){
        for(size_t i=0;i<state.favs.size();i++)
            if(state.favs[i].chId==c.id){
                state.favs.erase(state.favs.begin()+i);
                touchState(); audio.sfx.trigger(Sfx::BACK); toast("Removed from favorites",Pal::dim);
                if(screen==SCR_LIB)rebuildRows(true);
                return;
            }
        state.favs.push_back({c.id,c.title,c.place,c.placeId,c.country,
                              c.url.empty()?rgStream(c.id):c.url,c.cc,c.lat,c.lon});
        touchState(); audio.sfx.trigger(Sfx::FAV); toast("Added to favorites",Pal::liveSoft);
        if(screen==SCR_LIB)rebuildRows(true);
    }
    void assignPreset(int i){
        if(!now.valid){ toast("Nothing playing",Pal::dim); return; }
        Preset& p=state.presets[i];
        p.set=true; p.chId=now.chId; p.title=now.title; p.place=now.place; p.placeId=now.placeId;
        p.country=now.country; p.cc=now.cc; p.lat=now.lat; p.lon=now.lon; p.url=now.url;
        touchState(); audio.sfx.trigger(Sfx::FAV);
        toast("Preset "+std::to_string(i+1)+" · "+now.title,Pal::warm);
        rebuildRows(true);
    }
    void randomWave(){
        if(randomBusy||places.empty())return;
        if(randomSlot.busy.exchange(true))return;
        randomBusy=true; audio.sfx.trigger(Sfx::SWEEP);
        toast("Catching a random wave…",Pal::accentSoft);
        std::vector<Place> pool;
        for(auto& p:places)if(p.size>0)pool.push_back(p);
        if(pool.empty()){ randomSlot.busy=false; randomBusy=false; return; }
        Place pick=pool[randIndex(pool.size())];
        worker.post([this,pick]{
            RandomRes r;
            CityRes cr=fetchCity(pick);
            std::vector<Channel> ok;
            for(auto& c:cr.ch)if(!brokenCh.count(c.id))ok.push_back(c);
            if(!ok.empty())r.ch=ok[randIndex(ok.size())];
            else r.err=cr.err.empty()?"no playable stations":cr.err;
            randomSlot.put(std::move(r));
        },true);
    }
    void doSearch(){
        if(query.size()<2){ hits.clear(); searchLoading=false; rebuildRows(false); return; }
        if(searchSlot.busy.load()){ searchWant=query; searchWantSet=true; searchLoading=true; return; }
        if(searchSlot.busy.exchange(true))return;
        searchLoading=true; searchRunning=query;
        std::string q=query;
        worker.post([this,q]{ std::string e; searchSlot.put(fetchSearch(q,e)); },true);
    }
    void openInput(){ query=swkbdInput(query,"Search stations and cities"); doSearch(); }
    void applySetting(int id){
        if(id==0){ state.uiSounds=!state.uiSounds; audio.sfx.uiOn=state.uiSounds; }
        else if(id==1){ state.pauseHiss=!state.pauseHiss; audio.sfx.hissOn=state.pauseHiss; }
        else if(id==2){ state.autoNight=!state.autoNight; if(!state.autoNight)nightMode=false; }
        else if(id==3){ cycleSleep(); rebuildRows(true); return; }
        else if(id==4){
            state.volume=state.volume>=.99f?.2f:clampf(state.volume+.2f,0,1);
            player.volume=state.volume; osdVol=1.6f;
        }
        else if(id==5){ audio.sfx.muted=!audio.sfx.muted.load(); }
        touchState(); audio.sfx.trigger(Sfx::POP); rebuildRows(true);
    }
    void activate(){
        if(selRows.empty()||selIdx<0||selIdx>=(int)selRows.size())return;
        Row& r=rows[selRows[selIdx]];
        if(r.kind==1){ if(!r.ch.id.empty())play(r.ch); }
        else if(r.kind==2){ const Place* p=placeById(r.placeId); if(p)openCity(*p); }
        else if(r.kind==3)applySetting(r.setting);
        else if(r.kind==4){
            const Preset& p=state.presets[r.slot];
            if(p.set){
                Channel c{p.chId,p.title,p.url,p.placeId,p.place,p.country,p.cc,p.lat,p.lon};
                play(c); globe.flyTo(p.lat,p.lon);
            } else assignPreset(r.slot);
        }
else if(r.kind==6)pcShow(curCity);
    }
    void moveSel(int d){
        scrollManual=false;
        int n=(int)selRows.size(); if(n<=0)return;
        int old=selIdx; selIdx=(selIdx+d+n)%n;
        if(selIdx!=old)audio.sfx.trigger(Sfx::TICK);
    }
    void switchTab(int d){
        scrollManual=false; scrollVel=0;
        if(screen!=SCR_LIB)return;
        libTab=(libTab+d+4)%4;
        if(libTab==LT_HIST)histView=player.hist();
        audio.sfx.trigger(Sfx::TICK); rebuildRows(false);
    }
    void favSelected(){
        if(screen==SCR_LIB&&libTab==LT_PRESET&&!selRows.empty()&&selIdx<(int)selRows.size()){
            Row& r=rows[selRows[selIdx]];
            if(r.kind==4){ assignPreset(r.slot); return; }
        }
        if(!selRows.empty()&&selIdx<(int)selRows.size()){
            Row& r=rows[selRows[selIdx]];
            if(r.kind==1){ toggleFav(r.ch); return; }
        }
        if(now.valid){
            Channel c{now.chId,now.title,now.url,now.placeId,now.place,now.country,now.cc,now.lat,now.lon};
            toggleFav(c);
        }
    }
    void pressConfirm(){
        if(cleanMode)return;
        if(placesError){ placesError=false; startPlaces(); return; }
        if(screen==SCR_GLOBE){
            if(globe.nearest>=0&&(size_t)globe.nearest<places.size())openCity(places[globe.nearest]);
            return;
        }
        if(screen==SCR_SEARCH&&selRows.empty()){ openInput(); return; }
        activate();
    }
    void pressBack(){
        if(homeMode)return;
        if(cleanMode){ setClean(false); return; }
        if(screen==SCR_GLOBE){ enterHome(); return; }
        screen=SCR_GLOBE;
        rebuildRows(false); audio.sfx.trigger(Sfx::BACK);
    }
    void updateIdle(float dt){
        pollJobs(); pollAsync();
        clockTimer-=dt; if(clockTimer<=0){clockTimer=1.f;refreshClocks();}
        if(player.state==Player::PLAYING){
            airAcc+=dt;
            if(airAcc>=1.f){int sec=(int)airAcc;airAcc-=sec;pp.airSec+=sec;airSaveT+=sec;
                if(airSaveT>=30){airSaveT=0;ppSave(pp);}}
        }
        stTick(dt);
        if(sleepMin>0&&sleepLeft>0){
            sleepLeft-=dt;
            if(sleepLeft<=0){sleepLeft=0;sleepMin=0;player.stop();now.valid=false;nowGta=false;
                globe.setSelected("");globe.setSelPoint(0,0,false);}
        }
        float ft=1.f;
        if(sleepMin>0&&sleepLeft>0&&sleepLeft<60.f)ft=clampf(sleepLeft/60.f,0,1);
        audio.fade=lerpf(audio.fade.load(),ft,1-powf(.02f,dt));
        if(stateDirty){saveTimer-=dt;if(saveTimer<=0)flushState();}
        bool wantMedia=now.valid&&(player.state==Player::PLAYING||player.state==Player::BUFFERING);
        if(wantMedia!=mediaOn){mediaOn=wantMedia;appletSetMediaPlaybackState(mediaOn);}
        t+=dt;
    }
    void update(float dt){
        pollJobs(); pollAsync(); updateEQ(dt); sky.update(dt); cleanUpdate(dt);
        pcUpdate(dt);
        gtaUpdate(dt);
        gtaCarUpdate(dt);
        {
            float tgt=homeMode?1.f:0.f;
            float spd=homeMode?4.5f:5.0f;
            if(homeMix<tgt)      homeMix=std::min(tgt,homeMix+dt*spd);
            else if(homeMix>tgt) homeMix=std::max(tgt,homeMix-dt*spd);
        }
        home.update(dt,homeMode);
        if(homeMode){
            globe.yaw+=dt*.018f;
            globe.idle=0;
            sky.nextShoot-=dt*1.6f;
            if(!globe.stOn)globe.stNext-=dt*4.f;
        }
        clockTimer-=dt;
        if(clockTimer<=0){ clockTimer=1.f; refreshClocks(); }
        idleT+=dt;
if(state.autoNight&&!cleanMode&&!homeMode&&!pcOpen&&!passportOpen&&!gtaMode&&now.valid&&player.state==Player::PLAYING&&idleT>600.f)nightMode=true;
if(!state.autoNight||cleanMode||homeMode||pcOpen||passportOpen||gtaMode)nightMode=false;
        nightMix=lerpf(nightMix,nightMode?1.f:0.f,1-powf(.004f,dt));
        if(sleepMin>0&&sleepLeft>0){
            sleepLeft-=dt;
            if(sleepLeft<=0){
                sleepLeft=0; sleepMin=0; player.stop(); now.valid=false; nowGta=false; globe.setSelected(""); globe.setSelPoint(0,0,false);
                nightMode=!cleanMode&&!homeMode; toast("Sleep timer ended · good night",Pal::warm);
            }
        }
        float ft=1.f;
        if(sleepMin>0&&sleepLeft>0&&sleepLeft<60.f)ft=clampf(sleepLeft/60.f,0,1);
        audio.fade=lerpf(audio.fade.load(),ft,1-powf(.02f,dt));
        if(stateDirty){ saveTimer-=dt; if(saveTimer<=0)flushState(); }
        if(osdVol>0)osdVol-=dt;
        if(player.state==Player::BUFFERING)tuneAnim+=dt; else tuneAnim=0;
		if(player.state==Player::PLAYING){
    airAcc+=dt;
    if(airAcc>=1.f){ int s=(int)airAcc; airAcc-=s; pp.airSec+=s; airSaveT+=s;
        if(airSaveT>=30){ airSaveT=0; ppSave(pp); } }
}
        stTick(dt);
        stAnimUpdate(dt);
        if(homeMode){
            if(pad){
                float lx=SDL_GameControllerGetAxis(pad,SDL_CONTROLLER_AXIS_LEFTX)/32768.f;
                if(!home.latch&&fabsf(lx)>.55f){ home.move(lx>0?1:-1); audio.sfx.trigger(Sfx::TICK); home.latch=true; }
                if(fabsf(lx)<.30f)home.latch=false;
            }
        } else if(!globe.locked&&!nightMode&&!gtaMode&&(screen==SCR_GLOBE||cleanMode)){
            float rx=0,ry=0;
            float zk=globe.inputK();
            if(pad){
                float lx=SDL_GameControllerGetAxis(pad,SDL_CONTROLLER_AXIS_LEFTX)/32768.f;
                float ly=SDL_GameControllerGetAxis(pad,SDL_CONTROLLER_AXIS_LEFTY)/32768.f;
                if(fabsf(lx)>.22f)rx-=lx*1.5f;
                if(fabsf(ly)>.22f)ry-=ly*1.f;
            }
            if(rx||ry){ globe.flying=false; globe.idle=0;
                globe.yaw+=rx*dt*zk; globe.pitch=clampf(globe.pitch+ry*dt*zk,-1.35f,1.35f); idleT=0; }
        }
        if(br&&!cleanMode&&!homeMode&&!gtaMode){ globe.zoomHold(1,dt); idleT=0; }
        if(bl&&!cleanMode&&!homeMode&&!gtaMode){ globe.zoomHold(-1,dt); idleT=0; }
        if(pad){
            float zl=SDL_GameControllerGetAxis(pad,SDL_CONTROLLER_AXIS_TRIGGERLEFT)/32768.f;
            float zr=SDL_GameControllerGetAxis(pad,SDL_CONTROLLER_AXIS_TRIGGERRIGHT)/32768.f;
            if(zl>.3f){ player.volume=clampf(player.volume-.5f*dt,0,1); state.volume=player.volume; osdVol=1.6f; idleT=0; touchState(); }
            if(zr>.3f){ player.volume=clampf(player.volume+.5f*dt,0,1); state.volume=player.volume; osdVol=1.6f; idleT=0; touchState(); }
        }
        globe.update(dt);
        panel=lerpf(panel,(screen!=SCR_GLOBE&&!homeMode)?1.f:0.f,1-powf(.00002f,dt));
        float listH=panelH()-124;
        float k=1-powf(.00002f,dt);
        if(scrollManual){
            scroll+=scrollVel*dt;
            scrollVel*=powf(.02f,dt);
            if(fabsf(scrollVel)<4.f)scrollVel=0;
            scroll=clampf(scroll,0.f,listScrollMax);
            focusTarget=focusY;
            if(!selRows.empty()&&selIdx<(int)selRows.size())focusTarget=rows[selRows[selIdx]].y;
            focusY=lerpf(focusY,focusTarget,k);
        }else{
            scrollTarget=0; focusTarget=focusY;
            if(!selRows.empty()&&selIdx<(int)selRows.size()){
                Row& r=rows[selRows[selIdx]];
                scrollTarget=clampf(r.y-listH/2+r.h/2,0.f,std::max(0.f,rowsH-listH));
                focusTarget=r.y;
            }
            scroll=lerpf(scroll,scrollTarget,k);
            focusY=lerpf(focusY,focusTarget,k);
        }
        for(auto& to:toasts)to.age+=dt;
        toasts.erase(std::remove_if(toasts.begin(),toasts.end(),[](const Toast& x){return x.age>4.5f;}),toasts.end());
        t+=dt;
        bool wantMedia=now.valid&&(player.state==Player::PLAYING||player.state==Player::BUFFERING);
        if(wantMedia!=mediaOn){ mediaOn=wantMedia; appletSetMediaPlaybackState(mediaOn); }
    }

    void enterRadio(){
        exitGta();
        if(!homeMode)return;
        homeMode=false; home.latch=false;
        globe.home=false; globe.targetR=Globe::DEF_R;
        globe.zoomIdle=0; globe.idle=0; globe.yawV=0; globe.pitchV=0;
        screen=SCR_GLOBE; rebuildRows(false);
        audio.sfx.trigger(Sfx::POP);
    }
    void enterHome(){
        exitGta();
        if(homeMode)return;
        if(cleanMode)setClean(false);
        homeMode=true; home.latch=false;
        screen=SCR_GLOBE; rebuildRows(false);
        globe.home=true; globe.targetR=Globe::HOME_R; globe.zoomIdle=0; globe.flying=false;
        audio.sfx.trigger(Sfx::BACK);
    }
    void homeActivate(){
        if(home.sel==0){openPassport();return;}
        if(home.sel==1){enterRadio();return;}
        if(home.sel==2){enterGta();return;}
    }
    void drawAurora(float A){
        if(A<.02f)return;
        float settle=clampf((globe.R-500.f)/380.f,0.f,1.f);
        A*=settle; if(A<.02f)return;
        SDL_RenderSetClipRect(ren,nullptr);
        SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
        const int NC=48;
        const float cx=globe.center.x,cy=globe.center.y,R=std::max(1.f,globe.R);
        const SDL_Color rc[3]={C(80,235,165),C(95,175,255),C(175,125,255)};
        const int ba[3]={52,38,28};
        const float TOP=-240.f,BOT=(float)H+240.f;

        auto limb=[&](float x)->float{
            float dx=x-cx;
            if(dx<-R)dx=-R; else if(dx>R)dx=R;
            float s=R*R-dx*dx; if(s<0.f)s=0.f;
            return clampf(cy-sqrtf(s),TOP,BOT);
        };
        auto hgt=[&](float x,int r)->float{
            return (74.f+42.f*r)+34.f*sinf(x*.0055f+t*(.34f+.11f*r)+r*2.1f)
                                +22.f*sinf(x*.0131f-t*.27f+r);
        };
        auto amp=[&](float x,int r)->float{
            return .62f+.38f*sinf(x*.0087f+t*.55f+r*1.7f);
        };
        auto q4=[&](float x0,float x1,float t0,float b0,float t1,float b1,
                    SDL_Color c0t,SDL_Color c0b,SDL_Color c1t,SDL_Color c1b){
            int p0=b.vert(x0,t0,c0t),p1=b.vert(x1,t1,c1t),p2=b.vert(x1,b1,c1b),p3=b.vert(x0,b0,c0b);
            b.tri(p0,p1,p2); b.tri(p0,p2,p3);
        };

        for(int i=0;i<NC;i++){
            float x0=W*i/(float)NC,x1=W*(i+1)/(float)NC;
            float l0=limb(x0),l1=limb(x1);
            if(l0>=BOT&&l1>=BOT)continue;
            for(int r=0;r<3;r++){
                float h0=hgt(x0,r),h1=hgt(x1,r);
                int a0=(int)(ba[r]*amp(x0,r)*A),a1=(int)(ba[r]*amp(x1,r)*A);
                if(a0<=0&&a1<=0)continue;
                float m0=l0-h0*.45f,m1=l1-h1*.45f;
                q4(x0,x1,l0-h0,m0,l1-h1,m1,
                   alpha(rc[r],0),alpha(rc[r],a0),alpha(rc[r],0),alpha(rc[r],a1));
                q4(x0,x1,m0,l0+6.f,m1,l1+6.f,
                   alpha(rc[r],a0),alpha(rc[r],(int)(a0*.45f)),
                   alpha(rc[r],a1),alpha(rc[r],(int)(a1*.45f)));
            }
            int g0=(int)(46*amp(x0,0)*A),g1=(int)(46*amp(x1,0)*A);
            q4(x0,x1,l0-30.f,l0+8.f,l1-30.f,l1+8.f,
               alpha(C(150,205,255),0),alpha(C(150,205,255),g0),
               alpha(C(150,205,255),0),alpha(C(150,205,255),g1));
            SDL_Color rim=alpha(C(170,220,255),(int)(72*A));
            q4(x0,x1,l0-1.1f,l0+1.1f,l1-1.1f,l1+1.1f,rim,rim,rim,rim);
        }
        b.flush(ren);
    }
    void drawHome(float A){
        if(A<.02f)return;
        auto AL=[&](int a){ return (int)clampf(a*A,0.f,255.f); };
        float ti=easeOut(clampf(home.intro/.6f,0.f,1.f));
{
    float ty=24.f-(1.f-ti)*18.f;
    int wR=tw(F42,"RADIO",true);
    int wN=tw(F24,"NX",true)+24;
    float sx=W/2.f-(wR+14.f+wN)*.5f;
    txt(F42,"RADIO",(int)sx,(int)ty,alpha(Pal::text,AL(255)),true);
    float bx=sx+wR+14.f,by=ty+8.f;
    b.rrect(bx,by,(float)wN,36.f,6.f,alpha(Pal::accent,AL(255))); b.flush(ren);
    txt(F24,"NX",(int)(bx+wN*.5f),(int)(by+4.f),alpha(C(255,255,255),AL(255)),true,1);
}
        static std::vector<Vec2> ol;
        const float D=26.f;
        for(int i=0;i<HomeMenu::N;i++){
            const HomeMenu::Card& k=home.c[i];
            float ia=easeOut(clampf((home.intro-i*.10f)/.55f,0.f,1.f));
            float a=A*ia; if(a<.02f)continue;
            auto A2=[&](int v){ return (int)clampf(v*a,0.f,255.f); };
            float bob=sinf(t*.85f+k.ph)*(4.f+2.5f*k.glow);
            float roll=sinf(t*.55f+k.ph)*.018f;
            float yaw=k.ty+sinf(t*.42f+k.ph*1.7f)*.03f;
            float pit=k.tx+sinf(t*.63f+k.ph)*.02f;
            float sc=k.sc*(.88f+.12f*ia)*(.97f+.03f*A);
            float cx=k.cx+k.push;
            float cy=k.cy+k.lift+bob+(1.f-ia)*70.f-(1.f-A)*36.f;
            float w=k.w,h=k.h;
            if(k.glow>.02f)
                HomeMenu::softEllipse(b,cx,cy+6.f,w*.78f*sc,h*.72f*sc,alpha(Pal::accent,(int)(46*k.glow*a)),26);
            float sh=1.f+k.lift/-40.f;
            HomeMenu::softEllipse(b,cx+6.f,cy+h*.5f*sc+34.f,w*.44f*sc*sh,10.f*sh,alpha(C(0,0,0),(int)(120*a)),24);
            b.flush(ren);
            HomeMenu::Xf X; X.set(cx,cy,sc,pit,yaw,roll);
            HomeMenu::outline(w,h,20.f,ol);
            int n=(int)ol.size();
            Vec2 F[32],Bp[32];
            for(int q=0;q<n;q++){ F[q]=X.p(ol[q].x,ol[q].y,0.f); Bp[q]=X.p(ol[q].x,ol[q].y,D); }
            Vec2 fc=X.p(0,0,0.f),bc=X.p(0,0,D);
            SDL_Color top=mixc(C(34,38,50),C(46,60,92),k.glow*.55f);
            SDL_Color bot=mixc(C(13,15,21),C(16,22,38),k.glow*.55f);
            SDL_Color side=mixc(C(7,8,12),C(10,14,26),k.glow*.5f);
            { int base=b.vert(bc.x,bc.y,alpha(side,A2(255))),first=-1,prev=-1;
              for(int q=0;q<n;q++){ int id=b.vert(Bp[q].x,Bp[q].y,alpha(side,A2(255)));
                  if(first<0)first=id; if(prev>=0)b.tri(base,prev,id); prev=id; }
              b.tri(base,prev,first); }
            for(int q=0;q<n;q++){
                int q2=(q+1)%n;
                float shade=clampf(.5f-ol[q].y/h,0.f,1.f);
                SDL_Color c2=alpha(mixc(side,C(26,30,42),shade*.55f),A2(255));
                b.quad(F[q].x,F[q].y,F[q2].x,F[q2].y,Bp[q2].x,Bp[q2].y,Bp[q].x,Bp[q].y,c2);
            }
            { int base=b.vert(fc.x,fc.y,alpha(mixc(top,bot,.5f),A2(255)));
              int ids[32];
              for(int q=0;q<n;q++){ float u=clampf(ol[q].y/h+.5f,0.f,1.f);
                  ids[q]=b.vert(F[q].x,F[q].y,alpha(mixc(top,bot,u),A2(255))); }
              for(int q=0;q<n;q++)b.tri(base,ids[q],ids[(q+1)%n]); }
            b.flush(ren);
            SDL_Color edge=mixc(alpha(Pal::wire,60),Pal::accentSoft,k.glow);
            for(int q=0;q<n;q++){ int q2=(q+1)%n;
                b.line(F[q].x,F[q].y,F[q2].x,F[q2].y,1.6f,alpha(edge,A2(200))); }
            b.line(F[n-1].x,F[n-1].y,F[0].x,F[0].y,2.f,alpha(Pal::wire,A2(34)));
            b.flush(ren);
            float ix=fc.x-yaw*26.f,iy=fc.y-pit*22.f-6.f,is=44.f*sc;
            SDL_Color ic=alpha(mixc(Pal::dim,Pal::text,k.glow),A2(255));
            SDL_Color ac=alpha(mixc(Pal::faint,Pal::accentSoft,k.glow),A2(255));
            if(k.icon==0)HomeIcon::passport(b,ix,iy,is,ic,ac);
            else if(k.icon==1)HomeIcon::radio(b,ix,iy,is,ic,ac);
            else HomeIcon::gta(b,ix,iy,is,ic,ac);
            b.flush(ren);
            int ly=(int)(cy+h*.5f*sc+30.f);
            txt(F24,k.name,(int)cx,ly,alpha(mixc(Pal::dim,Pal::text,k.glow),A2(255)),true,1);
            hit(H_HOME,(int)(cx-w*.5f*sc),(int)(cy-h*.5f*sc),(int)(w*sc),(int)(h*sc+40),i);
        }
    }

void pcFreePhoto(){ if(pcPhoto.tex){SDL_DestroyTexture(pcPhoto.tex);pcPhoto.tex=nullptr;} pcPhoto.w=pcPhoto.h=0; }
void pcShow(const Place& p){
    Card c; c.id=p.id; c.city=p.title; c.country=p.country; c.cc=isoForCountry(p.country);
    c.lat=p.lat; c.lon=p.lon;
    c.variant=(int)(pcHash(c.id)%12);
    c.ts=(long long)time(nullptr);
    if(!pcCurValid||pcCur.id!=c.id){ pcFreePhoto(); pcCur=c; pcCurValid=true; pcFaceKey.clear(); pcWant(); }
    pcOpen=true; audio.sfx.trigger(Sfx::POP);
}
void pcWant(){
    if(!pcCurValid||pcPhoto.tex)return;
    requestFlag(pcCur.cc.empty()?isoForCountry(pcCur.country):pcCur.cc);
    if(pcCur.photoState==2&&t-pcCur.failT<120.f)return;
    if(pcSlot.busy.exchange(true))return;
    Card c=pcCur;
    pcWorker.post([this,c]{
        PcPhotoRes r; r.id=c.id;
        std::string url=c.photoUrl,note,body;
        if(url.empty())url=pcFindPhoto(c,note);
        if(!url.empty()){ std::string e; body=httpGet(url,e,20,false,false,UA_WIKI);
            if(body.size()>1024)r.url=url; else body.clear(); }
        if(!body.empty())r.ok=decodeImageFit(body,384,r.rgba,r.w,r.h);
        pcSlot.put(std::move(r));
    });
}
void pcRetry(){
    if(!pcCurValid)return;
    pcCur.photoState=0; pcCur.photoUrl.clear(); pcCur.failT=-1e9f;
    pcFreePhoto(); pcFaceKey.clear(); pcWant();
    toast("Looking for a photo again…",Pal::accentSoft);
}
void pcClose(){ if(!pcOpen)return; pcOpen=false; audio.sfx.trigger(Sfx::BACK); }
void pcUpdate(float dt){
    ppUpdate(dt);
    pcMix=lerpf(pcMix,pcOpen?1.f:0.f,1-powf(.0015f,dt));
    PcPhotoRes r;
    if(pcSlot.take(r)){
        if(pcCurValid&&r.id==pcCur.id){
            pcCur.photoState=r.ok?1:2;
            if(!r.ok)pcCur.failT=t;
            if(!r.url.empty())pcCur.photoUrl=r.url;
            if(r.ok&&r.w>0&&r.h>0){
                pcFreePhoto();
                pcPhoto.tex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,r.w,r.h);
                if(pcPhoto.tex){
                    SDL_UpdateTexture(pcPhoto.tex,nullptr,r.rgba.data(),r.w*4);
                    SDL_SetTextureBlendMode(pcPhoto.tex,SDL_BLENDMODE_BLEND);
                    pcPhoto.w=r.w; pcPhoto.h=r.h; pcFaceKey.clear();
                }
            }
        }
    }
}

void ppEnsure(){
    bool ch=false;
    if(pp.number.empty()){ char nb[16];
snprintf(nb,16,"NX-%04u-%04u",(unsigned)(rngRef()()%10000),(unsigned)(rngRef()()%10000));
        pp.number=nb; ch=true; }
    if(pp.issued==0){ pp.issued=(long long)time(nullptr); ch=true; }
    if(pp.nick.empty()){ pp.nick="Traveler"; ch=true; }
    if(ch)ppSave(pp);
}
void ppRecord(const Channel& c){
    pp.stations++;
    std::string key=c.placeId.empty()?("n:"+lowerStr(c.place)):c.placeId;
    uint32_t h=pcHash(key);
    if(std::find(pp.cityIds.begin(),pp.cityIds.end(),h)==pp.cityIds.end()){
        if(pp.cityIds.size()<1024)pp.cityIds.push_back(h);
        pp.cities++;
    }
    std::string iso=lowerStr(c.cc.empty()?isoForCountry(c.country):c.cc);
    std::string ck=iso.empty()?("n:"+lowerStr(c.country)):iso;
    if(!ck.empty()&&pp.countries.size()<240&&
       std::find(pp.countries.begin(),pp.countries.end(),ck)==pp.countries.end())
        pp.countries.push_back(ck);
    if(!ck.empty()&&ck.rfind("n:",0)!=0)requestFlag(ck);
    ppSave(pp);
}
void ppLoadMiiOverride(){
    std::string mb;
    if(!readFile(gAppDir+"mii.png",mb)&&!readFile(gCacheDir+"mii.png",mb))return;

    std::vector<uint8_t> rgba; int w=0,h=0;
    if(!decodeImageFit(mb,256,rgba,w,h)||w<=0||h<=0)return;

    if(miiTex){SDL_DestroyTexture(miiTex);miiTex=nullptr;}
    miiTex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,w,h);
    if(miiTex){
        SDL_UpdateTexture(miiTex,nullptr,rgba.data(),w*4);
        SDL_SetTextureBlendMode(miiTex,SDL_BLENDMODE_BLEND);
    }
    pgKey.erase(0);
}
void openPassport(){ ppEnsure(); ppLoadMiiOverride(); ppPrefetchFlags(); passportOpen=true; ppChimed=false; audio.sfx.trigger(Sfx::PAGE); }
void closePassport(){ passportOpen=false; ppSave(pp); audio.sfx.trigger(Sfx::BACK); }
void ppFreePages(){
    for(auto& kv:pgTex)if(kv.second)SDL_DestroyTexture(kv.second);
    pgTex.clear();pgKey.clear();pgUse.clear();
    if(coverTex){SDL_DestroyTexture(coverTex);coverTex=nullptr;}
}
void ppTurnPage(int d){
    if(!passportOpen||ppTurning||passportMix<.9f)return;
    int ns=ppSpread+d;
    if(ns<0||ns>=ppSpreadCount()){audio.sfx.trigger(Sfx::BACK);return;}
    ppDir=d;ppTurn=0;ppTurning=true;audio.sfx.trigger(Sfx::PAGE);
}
void ppUpdate(float dt){
    passportMix=lerpf(passportMix,passportOpen?1.f:0.f,1-powf(.0016f,dt));
    if(passportOpen&&passportMix>.93f&&!ppChimed){ ppChimed=true; audio.sfx.trigger(Sfx::CHIME); }
    if(!passportOpen&&passportMix<.05f)ppChimed=false;
    if(ppTurning){
        ppTurn+=dt/.62f;
        if(ppTurn>=1.f){ ppTurn=0; ppTurning=false; ppSpread+=ppDir; ppDir=0; audio.sfx.trigger(Sfx::TICK); }
    }
    if(!passportOpen&&passportMix<.02f){ ppSpread=0; ppTurn=0; ppDir=0; ppTurning=false; }
    blinkT-=dt;
    if(blinkT<=0){ blinkP+=dt/.22f; if(blinkP>=1.f){ blinkP=0; blinkT=frand(2.4f,5.4f); } }
}
void mrzLines(std::string& l1,std::string& l2){
    auto cln=[](const std::string& s){ std::string o;
        for(unsigned char c:s){ if(c>='a'&&c<='z')o+=(char)(c-'a'+'A');
            else if(c>='A'&&c<='Z')o+=(char)c; else o+='<'; } return o; };
    std::string n=cln(pp.nick); if(n.empty())n="TRAVELER";
    l1="P<RNX"+n+"<<"; l1=(l1+std::string(44,'<')).substr(0,44);
    std::string d; for(char c:pp.number)if(c>='0'&&c<='9')d+=c;
    d=(d+std::string(9,'<')).substr(0,9);
    tm it=tmUtc((time_t)pp.issued); char ds[8];
    snprintf(ds,8,"%02d%02d%02d",(it.tm_year+1900)%100,it.tm_mon+1,it.tm_mday);
    l2=d+"RNX"+ds+"0"+"991231"+"0EARTH"; l2=(l2+std::string(44,'<')).substr(0,44);
}
void drawMrzLine(const std::string& s,float x,float y,float avail,SDL_Color col){
    float ws=0;
    for(size_t i=0;i<s.size();){ size_t st=i; utf8Next(s,i); ws+=(float)tw(F15,s.substr(st,i-st),true); }
    float tr=s.size()>1?clampf((avail-ws)/(float)(s.size()-1),0.f,2.f):0.f;
    pcSpaced(s,x,y,F15,tr,col);
}
void spacedCenter(const std::string& s,float cx,float y,int fi,float tr,SDL_Color c){
    float w=(float)tw(fi,s,true)+tr*(float)(s.empty()?0:(int)s.size()-1);
    pcSpaced(s,cx-w*.5f,y,fi,tr,c);
}

void drawAvatar(float x,float y,float s){
    uint32_t sd=(uint32_t)(ppSeed^(ppSeed>>32));
    const SDL_Color skin[5]={C(255,226,198),C(246,210,178),C(226,186,152),C(198,154,120),C(162,118,90)};
    const SDL_Color hair[6]={C(58,44,38),C(108,74,50),C(196,142,72),C(44,46,58),C(152,62,52),C(230,208,170)};
    SDL_Color bg=hsv((float)(sd%360),.22f,.96f), sk=skin[sd%5], hr=hair[(sd/5)%6];
    SDL_Color shirt=hsv((float)((sd/11)%360),.42f,.78f);
    float cx=x+s*.5f, cy=y+s*.5f;
    b.rrect(x,y,s,s,s*.18f,bg);
    b.rrect2(cx-s*.30f,cy+s*.26f,s*.60f,s*.30f,s*.12f,s*.10f,shirt);
    b.circle(cx,cy-s*.10f,s*.27f,hr,24);
    if(sd%3==0){ b.circle(cx-s*.24f,cy-s*.02f,s*.07f,hr,10); b.circle(cx+s*.24f,cy-s*.02f,s*.07f,hr,10); }
    b.circle(cx,cy+s*.03f,s*.25f,sk,24);
    float bk=1.f-sinf((float)M_PI*clampf(blinkP,0.f,1.f))*.9f;
    SDL_Color ink=C(40,42,50);
    for(int k=0;k<2;k++){
        float ex=cx+(k?1.f:-1.f)*s*.10f;
        b.rrect(ex-s*.022f,cy-s*.01f,s*.044f,std::max(1.5f,s*.10f*bk),s*.02f,ink);
        if(bk>.5f)b.circle(ex+s*.008f,cy+s*.005f,s*.012f,alpha(C(255,255,255),190),6);
        b.line(ex-s*.035f,cy-s*.075f,ex+s*.035f,cy-s*.085f,s*.016f,alpha(hr,160));
    }
    b.circle(cx-s*.16f,cy+s*.09f,s*.045f,alpha(C(255,140,120),60),10);
    b.circle(cx+s*.16f,cy+s*.09f,s*.045f,alpha(C(255,140,120),60),10);
    b.ring(cx,cy+s*.08f,s*.09f,s*.02f,C(70,50,46),12,(float)M_PI*.15f,(float)M_PI*.85f);
    if(sd%4==0){
        b.ring(cx-s*.10f,cy+s*.01f,s*.065f,s*.012f,alpha(ink,200),12);
        b.ring(cx+s*.10f,cy+s*.01f,s*.065f,s*.012f,alpha(ink,200),12);
        b.line(cx-s*.035f,cy+s*.01f,cx+s*.035f,cy+s*.01f,s*.012f,alpha(ink,200));
    }
}
void drawHologram(float cx,float cy){
    for(int k=0;k<3;k++)
        b.ring(cx,cy,16.f-k*4.f,2.2f,alpha(hsv(fmodf(t*40.f+k*120.f,360.f),.65f,.95f),85),20,t*.7f+k,t*.7f+k+4.6f);
    b.circle(cx+cosf(t*1.3f)*10.f,cy+sinf(t*1.3f)*10.f,2.2f,alpha(C(255,255,255),200),8);
}

void drawAvatarLive(float x,float y,float s,float a){
    if(a<.98f)return;
    if(miiTex){
        float br=1.f+.012f*sinf(t*1.3f);
        int d=(int)(s*br);
        SDL_Rect r{(int)(x+s*.5f-d*.5f),(int)(y+s*.5f-d*.5f),d,d};
        SDL_RenderCopy(ren,miiTex,nullptr,&r);
    } else drawAvatar(x,y,s);
    b.flush(ren);
    drawHologram(x+s-4,y+s-4); b.flush(ren);
}
void buildCover(){
    if(coverTex)return;
    coverTex=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,448,576);
    if(!coverTex)return;
    SDL_SetTextureBlendMode(coverTex,SDL_BLENDMODE_BLEND);
    b.flush(ren);
    SDL_SetRenderTarget(ren,coverTex);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,0,0,0,0); SDL_RenderClear(ren);
    const SDL_Color gold=C(232,198,124);
    b.rrect2(0,0,448,576,26,22,C(24,32,62));
    b.gradRect(3,3,442,300,alpha(C(64,84,140),70),alpha(C(64,84,140),0));
    b.gradRect(3,300,442,273,alpha(C(0,0,0),0),alpha(C(0,0,0),60));
    HomeIcon::frame(b,14,14,420,548,18,alpha(gold,150));
    HomeIcon::frame(b,22,22,404,532,14,alpha(gold,60));
    float ex=224,ey=212;
    b.ring(ex+2,ey+3,74,3,alpha(C(0,0,0),70),40);
    b.ring(ex,ey,74,3,alpha(gold,235),40);
    b.line(ex-74,ey,ex+74,ey,2,alpha(gold,180));
    b.line(ex-64,ey-34,ex+64,ey-34,1.6f,alpha(gold,120));
    b.line(ex-64,ey+34,ex+64,ey+34,1.6f,alpha(gold,120));
    for(int i=0;i<16;i++){
        float a0=(float)M_PI*2*i/16,a1=(float)M_PI*2*(i+1)/16;
        b.line(ex+cosf(a0)*33,ey+sinf(a0)*74,ex+cosf(a1)*33,ey+sinf(a1)*74,1.8f,alpha(gold,150));
    }
    b.circle(ex,ey,4,alpha(gold,220),10);
    txt(F15,"RADIO NX",224,52,alpha(gold,220),true,1);
    spacedCenter("WORLD PASSPORT",224,330,F30,4.f,alpha(gold,240));
    spacedCenter("ELECTRONIC PASSPORT",224,376,F15,2.f,alpha(gold,150));
    float bx=224,by=474;
    b.line(bx-24,by-16,bx+24,by-16,2,alpha(gold,200));
    b.line(bx-24,by+16,bx+24,by+16,2,alpha(gold,200));
    b.line(bx-24,by-16,bx-24,by+16,2,alpha(gold,200));
    b.line(bx+24,by-16,bx+24,by+16,2,alpha(gold,200));
    b.ring(bx,by,9,2,alpha(gold,210),16);
    b.line(bx-24,by,bx-9,by,2,alpha(gold,210));
    b.line(bx+9,by,bx+24,by,2,alpha(gold,210));
    b.flush(ren);
    SDL_SetRenderTarget(ren,nullptr);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
}

void stMotif(Batch& bb,int id,float x,float y,float s,SDL_Color c){
    auto L=[&](float a,float b,float d,float e,float th){bb.line(x+a*s,y+b*s,x+d*s,y+e*s,th,c);};
    auto Ci=[&](float a,float b,float r){bb.circle(x+a*s,y+b*s,r*s,c,10);};
    auto R=[&](float a,float b,float r){bb.ring(x+a*s,y+b*s,r*s,1.6f,c,14);};
    switch(id){
    case 2: L(0,.5f,0,-.5f,1.6f); L(0,-.1f,-.4f,-.4f,1.6f); L(0,-.1f,.4f,-.4f,1.6f);
            L(0,-.3f,-.3f,-.6f,1.6f); L(0,-.3f,.3f,-.6f,1.6f); L(-.4f,-.4f,-.2f,-.2f,1.6f); L(.4f,-.4f,.2f,-.2f,1.6f); break;
    case 3: for(int k=0;k<5;k++){float a=k*1.256f-1.57f;Ci(cosf(a)*.28f,sinf(a)*.28f,.18f);} Ci(0,0,.12f); break;
    case 4: L(-.3f,.4f,-.3f,-.1f,1.6f); L(.3f,.4f,.3f,-.1f,1.6f); L(-.3f,-.1f,0,-.4f,1.6f); L(.3f,-.1f,0,-.4f,1.6f);
            L(-.3f,-.1f,.3f,-.1f,1.6f); break;
    case 5: L(0,.4f,0,-.2f,1.6f); L(-.4f,-.2f,.4f,-.2f,1.6f); L(-.3f,-.4f,.3f,0,1.6f); L(.3f,-.4f,-.3f,0,1.6f); break;
    case 6: Ci(0,-.1f,.25f); L(-.2f,.1f,-.4f,.4f,1.6f); L(.2f,.1f,.4f,.4f,1.6f); L(.2f,-.2f,.5f,-.4f,1.6f); break;
    case 7: Ci(0,0,.25f); L(.2f,0,.5f,.1f,1.6f); L(-.2f,.2f,-.3f,.4f,1.6f); break;
    case 8: L(-.2f,.4f,-.2f,-.2f,1.6f); L(.2f,.4f,.2f,-.2f,1.6f); Ci(0,-.3f,.18f); L(-.2f,-.2f,.2f,-.2f,1.6f); break;
    case 9: L(-.4f,.4f,0,-.4f,1.6f); L(0,-.4f,.4f,.4f,1.6f); L(-.4f,.4f,.4f,.4f,1.6f); L(-.2f,.1f,.2f,.1f,1.2f); break;
    case 10: bb.ring(x,y,s*.4f,s*.12f,c,18,.6f,5.2f); Ci(.25f,-.2f,.08f); break;
    case 11: Ci(0,0,.2f); for(int k=0;k<8;k++){float a=k*.785f;L(cosf(a)*.3f,sinf(a)*.3f,cosf(a)*.5f,sinf(a)*.5f,1.4f);} break;
    case 12: for(int k=0;k<8;k++){float a=k*.785f;L(cosf(a)*.15f,sinf(a)*.15f,cosf(a)*.4f,sinf(a)*.4f,1.4f);} R(0,0,.15f); break;
    case 13: L(-.4f,.3f,.4f,.3f,1.6f); L(-.3f,.1f,.3f,.1f,1.6f); L(-.2f,-.1f,.2f,-.1f,1.6f);
             L(-.4f,.3f,-.2f,-.1f,1.6f); L(.4f,.3f,.2f,-.1f,1.6f); L(0,-.1f,0,-.4f,1.6f); break;
    case 14: L(-.15f,.4f,-.15f,-.3f,1.6f); L(.15f,.4f,.15f,-.3f,1.6f); L(-.15f,-.3f,.15f,-.3f,1.6f);
             L(-.25f,.1f,.25f,.1f,1.2f); L(0,-.3f,0,-.5f,1.6f); break;
    case 15: L(0,.4f,0,-.2f,1.6f); Ci(0,-.3f,.15f); L(-.2f,.4f,.2f,.4f,1.6f); break;
    case 16: L(-.4f,.3f,-.1f,-.3f,1.6f); L(-.1f,-.3f,.1f,.1f,1.6f); L(.1f,.1f,.3f,-.2f,1.6f); L(.3f,-.2f,.5f,.3f,1.6f); break;
    case 17: for(int k=0;k<3;k++){float yy=-.2f+k*.2f; L(-.4f,yy,-.2f,yy-.1f,1.4f); L(-.2f,yy-.1f,0,yy,1.4f);
             L(0,yy,.2f,yy-.1f,1.4f); L(.2f,yy-.1f,.4f,yy,1.4f);} break;
    case 18: bb.ring(x,y,s*.35f,s*.1f,c,14,.5f,5.8f); L(0,-.3f,0,.3f,1.2f); break;
    case 19: for(int k=0;k<6;k++){float a=k*1.047f;L(cosf(a)*.1f,sinf(a)*.1f,cosf(a)*.4f,sinf(a)*.4f,1.4f);} Ci(0,0,.12f); break;
    case 20: L(0,.4f,0,-.1f,2.f); L(-.3f,-.1f,.3f,-.1f,1.6f); Ci(-.2f,-.25f,.15f); Ci(.2f,-.25f,.15f); Ci(0,-.35f,.15f); break;
    case 21: for(int k=0;k<5;k++){float a=k*1.256f-1.57f;L(cosf(a)*.1f,sinf(a)*.1f,cosf(a)*.35f,sinf(a)*.35f,1.4f);} Ci(0,.1f,.12f); break;
    case 22: {int base=bb.vert(x,y,c),first=-1,prev=-1;
        for(int k=0;k<10;k++){float a=-1.57f+k*.628f,r=(k%2? .2f:.45f)*s;
        int id2=bb.vert(x+cosf(a)*r,y+sinf(a)*r,c); if(first<0)first=id2; if(prev>=0)bb.tri(base,prev,id2); prev=id2;}
        bb.tri(base,prev,first);} break;
    case 23: R(0,0,.35f); L(0,-.4f,0,.4f,1.2f); L(-.4f,0,.4f,0,1.2f); L(-.2f,-.2f,.2f,.2f,1.2f); break;
    case 24: L(-.4f,.1f,0,-.2f,1.6f); L(0,-.2f,.4f,.1f,1.6f); L(-.2f,.1f,0,.3f,1.6f); L(.2f,.1f,0,.3f,1.6f); break;
    case 26: Ci(-.15f,-.1f,.15f); Ci(.15f,-.1f,.15f); Ci(0,-.25f,.15f); L(0,0,0,.4f,1.6f); break;
    case 27: Ci(0,-.2f,.18f); L(-.2f,-.1f,.2f,-.1f,1.4f); L(0,0,0,.4f,1.6f); L(-.15f,.2f,.15f,.2f,1.2f); break;
    case 28: Ci(0,-.15f,.15f); for(int k=0;k<6;k++){float a=k*1.047f;L(cosf(a)*.15f,-.15f+sinf(a)*.15f,cosf(a)*.3f,-.15f+sinf(a)*.3f,1.2f);} L(0,0,0,.4f,1.6f); break;
    case 29: L(0,.4f,0,-.3f,2.f); L(0,-.1f,-.25f,-.1f,1.6f); L(-.25f,-.1f,-.25f,-.3f,1.6f);
             L(0,.1f,.25f,.1f,1.6f); L(.25f,.1f,.25f,-.1f,1.6f); break;
    case 30: L(0,.4f,0,-.2f,1.8f); for(int k=0;k<5;k++){float a=-.8f+k*.4f;L(0,-.2f,cosf(a)*.4f,-.2f+sinf(a)*.3f,1.4f);} break;
    case 31: for(int k=0;k<6;k++){float a=k*1.047f;L(0,0,cosf(a)*.4f,sinf(a)*.4f,1.2f);
             L(cosf(a)*.2f,sinf(a)*.2f,cosf(a+.4f)*.3f,sinf(a+.4f)*.3f,1.f);} break;
    case 32: L(-.4f,.1f,.4f,.1f,1.6f); L(-.3f,.1f,-.1f,.35f,1.6f); L(.3f,.1f,.1f,.35f,1.6f); L(0,.1f,0,-.35f,1.6f); L(0,-.35f,.25f,-.1f,1.4f); break;
    case 33: bb.ring(x,y,s*.3f,s*.08f,c,12,.3f,5.9f); L(.3f,0,.5f,-.15f,1.4f); L(.3f,0,.5f,.15f,1.4f); Ci(-.1f,-.05f,.05f); break;
    case 34: Ci(-.15f,-.1f,.18f); Ci(.15f,-.1f,.18f); Ci(-.1f,.15f,.12f); Ci(.1f,.15f,.12f); L(0,-.2f,0,.25f,1.4f); break;
    case 35: Ci(0,-.1f,.2f); L(-.2f,.1f,-.3f,.4f,1.6f); L(.2f,.1f,.3f,.4f,1.6f); Ci(.15f,-.3f,.12f); break;
    case 36: Ci(0,-.15f,.18f); L(-.2f,.05f,-.3f,.4f,1.6f); L(.2f,.05f,.3f,.4f,1.6f); L(.15f,-.25f,.35f,-.4f,1.4f); break;
    case 37: Ci(0,0,.25f); Ci(-.2f,-.2f,.1f); Ci(.2f,-.2f,.1f); Ci(0,.25f,.15f); break;
    case 38: Ci(0,0,.22f); L(-.2f,-.15f,-.4f,-.35f,1.6f); L(.2f,-.15f,.4f,-.35f,1.6f); L(-.15f,.2f,.15f,.2f,1.4f); break;
    case 39: bb.ring(x,y,s*.35f,1.6f,c,14,3.4f,6.f); L(-.25f,.2f,-.25f,-.2f,1.2f); L(0,.25f,0,-.3f,1.2f); L(.25f,.2f,.25f,-.2f,1.2f); break;
    case 40: L(-.35f,.3f,-.35f,-.2f,1.6f); L(.35f,.3f,.35f,-.2f,1.6f); L(-.35f,-.2f,.35f,-.2f,1.6f);
             L(-.35f,-.2f,-.25f,-.4f,1.4f); L(.35f,-.2f,.25f,-.4f,1.4f); L(-.1f,.3f,-.1f,.05f,1.4f); L(.1f,.3f,.1f,.05f,1.4f); break;
    case 45: L(-.4f,.3f,.4f,.3f,1.6f); L(-.3f,.1f,.3f,.1f,1.6f); L(-.2f,-.1f,.2f,-.1f,1.6f); L(0,-.1f,0,-.4f,1.6f); break;
    case 50: R(0,0,.3f); L(-.3f,-.2f,.3f,-.2f,1.4f); L(-.3f,.2f,.3f,.2f,1.4f); L(-.2f,-.2f,-.2f,.2f,1.2f); L(.2f,-.2f,.2f,.2f,1.2f); break;
    default: R(0,0,.35f); L(-.35f,0,.35f,0,1.2f); bb.ring(x,y,s*.17f,1.2f,c,10); break;
    }
}

struct StItem{ int kind; std::string t; int fi; float w,h; };

void stTextSize(int fi,const std::string& s,float& w,float& h){
    w=h=0.f;
    for(const Run& r:runs(fi,s)){
        TTF_Font* f=fonts.get(fi,r.fam); if(!f)continue;
        TTF_SetFontStyle(f,TTF_STYLE_BOLD);
        int ww=0,hh=0;
        if(TTF_SizeUTF8(f,r.text.c_str(),&ww,&hh)!=0)continue;
        w+=(float)ww; h=std::max(h,(float)hh);
    }
}

float stLineFit(const std::string& s,int fi,float cx,float cy,float maxW,float maxH,SDL_Color c){
    if(s.empty()||maxW<2.f||maxH<2.f)return 0.f;
    struct P{ TTF_Font* f; std::string t; float w,h; };
    std::vector<P> ps; float W=0,H=0;
    for(const Run& r:runs(fi,s)){
        TTF_Font* f=fonts.get(fi,r.fam); if(!f)continue;
        TTF_SetFontStyle(f,TTF_STYLE_BOLD);
        int w=0,h=0;
        if(TTF_SizeUTF8(f,r.text.c_str(),&w,&h)!=0||w<=0)continue;
        ps.push_back(P{f,r.text,(float)w,(float)h});
        W+=(float)w; H=std::max(H,(float)h);
    }
    if(ps.empty())return 0.f;
    float k=std::min(1.f,std::min(maxW/W,maxH/H));
    float x=cx-W*k*.5f;
    for(const P& p:ps){
        SDL_Surface* su=TTF_RenderUTF8_Blended(p.f,p.t.c_str(),c);
        if(su){
            SDL_Texture* tx=SDL_CreateTextureFromSurface(ren,su);
            if(tx){
                SDL_Rect d{(int)floorf(x),(int)floorf(cy-p.h*k*.5f),
                           (int)ceilf(p.w*k),(int)ceilf(p.h*k)};
                SDL_RenderCopy(ren,tx,nullptr,&d);
                SDL_DestroyTexture(tx);
            }
            SDL_FreeSurface(su);
        }
        x+=p.w*k;
    }
    return H*k;
}

void stArcFit(const std::string& s,float cx,float cy,float rx,float ry,
              float spanMax,bool bottom,int fi,SDL_Color c,float maxH)
{
    if(s.empty()||rx<3.f||ry<3.f||spanMax<.1f||maxH<3.f)return;
    struct G{ TTF_Font* f; std::string t; float w,h; };
    std::vector<G> gs; float W=0,H=0;
    size_t i=0;
    while(i<s.size()){
        size_t st=i; unsigned cp=utf8Next(s,i);
        std::string ch=s.substr(st,i-st);
        TTF_Font* f=fonts.get(fi,fonts.famFor(cp,fi)); if(!f)continue;
        TTF_SetFontStyle(f,TTF_STYLE_BOLD);
        int w=0,h=0; TTF_SizeUTF8(f,ch.c_str(),&w,&h);
        if(h<=0)continue;
        if(w<=0)w=(int)(h*.35f);
        gs.push_back(G{f,ch,(float)w,(float)h});
        W+=(float)w; H=std::max(H,(float)h);
    }
    if(gs.empty()||W<=0.f)return;
    float track=H*.08f;
    auto speed=[&](float a){
        float sa=sinf(a),ca=cosf(a);
        return std::max(2.f,sqrtf(rx*rx*sa*sa+ry*ry*ca*ca));
    };
    auto measure=[&](float k){
        float a=bottom?(float)M_PI*.5f:-(float)M_PI*.5f;
        float dir=bottom?-1.f:1.f,used=0.f;
        for(size_t q=0;q<gs.size();q++){
            float adv=(gs[q].w+(q+1<gs.size()?track:0.f))*k;
            float da=adv/speed(a+dir*adv*.5f/speed(a));
            a+=dir*da; used+=da;
        }
        return used;
    };
    float k=std::min(1.f,maxH/H);
    float span=measure(k);
    if(span>spanMax){ k*=spanMax/span; span=measure(k); }
    if(span>spanMax){ k*=spanMax/span; span=measure(k); }
    if(k<.3f)return;
    float a=bottom?((float)M_PI*.5f+span*.5f):(-(float)M_PI*.5f-span*.5f);
    float dir=bottom?-1.f:1.f;
    for(size_t q=0;q<gs.size();q++){
        const G& g=gs[q];
        float adv=(g.w+(q+1<gs.size()?track:0.f))*k;
        float da=adv/speed(a+dir*adv*.5f/speed(a));
        float mid=a+dir*da*.5f;
        if(g.t!=" "){
            SDL_Surface* su=TTF_RenderUTF8_Blended(g.f,g.t.c_str(),c);
            if(su){
                SDL_Texture* tx=SDL_CreateTextureFromSurface(ren,su);
                if(tx){
                    float px=cx+cosf(mid)*rx,py=cy+sinf(mid)*ry;
                    float dw=std::max(1.f,g.w*k),dh=std::max(1.f,g.h*k);
                    SDL_Rect d{(int)floorf(px-dw*.5f),(int)floorf(py-dh*.5f),
                               (int)ceilf(dw),(int)ceilf(dh)};
                    SDL_RenderCopyEx(ren,tx,nullptr,&d,mid*180.0/M_PI+90.0+(bottom?180.0:0.0),
                                     nullptr,SDL_FLIP_NONE);
                    SDL_DestroyTexture(tx);
                }
                SDL_FreeSurface(su);
            }
        }
        a+=dir*da;
    }
}

void stStack(std::vector<StItem>& it,float cx,float cy,float maxW,float maxH,
             float size,SDL_Color ink,int motif,SDL_Texture* ov)
{
    if(it.empty())return;
    float gap=size*.016f,H=gap*(float)(it.size()-1),W=0;
    for(const StItem& q:it){ H+=q.h; W=std::max(W,q.w); }
    if(H<=0.f||W<=0.f)return;
    float k=std::min(1.f,std::min(maxW/W,maxH/H));
    float y=cy-H*k*.5f;
    for(const StItem& q:it){
        float h=q.h*k;
        if(q.kind==0) stLineFit(q.t,q.fi,cx,y+h*.5f,maxW,h,ink);
        else if(q.kind==1){
            if(ov){
                float mr=h*.5f;
                SDL_Rect dd{(int)(cx-mr),(int)(y+h*.5f-mr),(int)(mr*2),(int)(mr*2)};
                SDL_RenderCopy(ren,ov,nullptr,&dd);
            } else stMotif(b,motif,cx,y+h*.5f,h*1.05f,ink);
            b.flush(ren);
        } else {
            float lw=std::min(maxW,q.w*k);
            b.line(cx-lw*.5f,y+h*.5f,cx+lw*.5f,y+h*.5f,std::max(1.f,size*.006f),alpha(ink,170));
            b.flush(ren);
        }
        y+=(q.h+gap)*k;
    }
}

std::string stCountryLabel(const Stamp& s,int fi,float maxW){
    std::string n=upperStr(trimStr(s.country.empty()?s.iso:s.country));
    if(n.rfind("N:",0)==0)n=trimStr(n.substr(2));
    if(n.empty())n=upperStr(s.iso);
    if((float)tw(fi,n,true)<=maxW)return n;
    static const char* A[5]={" AND ","REPUBLIC OF ","THE ","FEDERATION","REPUBLIC"};
    static const char* B[5]={" & ","","","FED.","REP."};
    for(int i=0;i<5;i++){
        size_t p;
        while((p=n.find(A[i]))!=std::string::npos)n.replace(p,strlen(A[i]),B[i]);
        n=trimStr(n);
        if((float)tw(fi,n,true)<=maxW)return n;
    }
    if((float)tw(fi,n,true)<=maxW*1.3f)return n;
    if(s.iso.size()>=2&&s.iso.rfind("n:",0)!=0)return upperStr(s.iso);
    return n;
}
SDL_Texture* stOverride(const std::string& iso){
    auto it=stOvr.find(iso); if(it!=stOvr.end())return it->second;
    if(stOvrTried.count(iso))return nullptr;
    stOvrTried.insert(iso);
    std::string body;
    std::string p1=gAppDir+"stamps/"+iso+".png",p2=gCacheDir+"stamps/"+iso+".png";
    if(!readFile(p1,body)&&!readFile(p2,body))return nullptr;
    std::vector<uint8_t> rgba; int w=0,h=0;
    if(!decodeImageFit(body,128,rgba,w,h)||w<=0)return nullptr;
    SDL_Texture* tx=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ABGR8888,SDL_TEXTUREACCESS_STATIC,w,h);
    if(!tx)return nullptr;
    SDL_UpdateTexture(tx,nullptr,rgba.data(),w*4);
    SDL_SetTextureBlendMode(tx,SDL_BLENDMODE_BLEND);
    stOvr[iso]=tx;
    if(stOvr.size()>12){auto old=stOvr.begin();SDL_DestroyTexture(old->second);stOvr.erase(old);}
    return tx;
}
SDL_Texture* stBake(const Stamp& s,int size){
    SDL_Texture* prev=SDL_GetRenderTarget(ren);
    SDL_Texture* tgt=SDL_CreateTexture(ren,SDL_PIXELFORMAT_RGBA32,SDL_TEXTUREACCESS_TARGET,size,size);
    if(!tgt){SDL_SetRenderTarget(ren,prev);return nullptr;}
    SDL_SetTextureBlendMode(tgt,SDL_BLENDMODE_BLEND);
    b.flush(ren);
    SDL_SetRenderTarget(ren,tgt);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,0,0,0,0);
    SDL_RenderClear(ren);

    const StampCDef& d=stDef(s.iso);
    StStyle sy=stStyle(s,d);
    uint32_t hs=pcHash("stamp|"+lowerStr(s.iso));
    SDL_Color ink=stInk(s,d);
    float u=size/192.f, cx=size*.5f, cy=size*.5f, R=size*.44f;
    float rx=R*sy.rx, ry=R*sy.ry, bw=std::max(1.3f,2.3f*u);

    auto ell=[&](float AX,float AY,float th,int seg){
        for(int i=0;i<seg;i++){
            float a0=2*(float)M_PI*i/seg,a1=2*(float)M_PI*(i+1)/seg;
            b.line(cx+cosf(a0)*AX,cy+sinf(a0)*AY,cx+cosf(a1)*AX,cy+sinf(a1)*AY,th,ink);
        }
    };
    auto poly=[&](int n,float AX,float AY,float rot,float th){
        for(int i=0;i<n;i++){
            float a0=rot+2*(float)M_PI*i/n,a1=rot+2*(float)M_PI*(i+1)/n;
            b.line(cx+cosf(a0)*AX,cy+sinf(a0)*AY,cx+cosf(a1)*AX,cy+sinf(a1)*AY,th,ink);
        }
    };
    auto boxf=[&](float AX,float AY,float cr,float th){
        if(cr<=.5f){
            b.line(cx-AX,cy-AY,cx+AX,cy-AY,th,ink); b.line(cx-AX,cy+AY,cx+AX,cy+AY,th,ink);
            b.line(cx-AX,cy-AY,cx-AX,cy+AY,th,ink); b.line(cx+AX,cy-AY,cx+AX,cy+AY,th,ink);
            return;
        }
        cr=std::min(cr,std::min(AX,AY));
        b.line(cx-AX+cr,cy-AY,cx+AX-cr,cy-AY,th,ink);
        b.line(cx-AX+cr,cy+AY,cx+AX-cr,cy+AY,th,ink);
        b.line(cx-AX,cy-AY+cr,cx-AX,cy+AY-cr,th,ink);
        b.line(cx+AX,cy-AY+cr,cx+AX,cy+AY-cr,th,ink);
        const float qx[4]={cx+AX-cr,cx+AX-cr,cx-AX+cr,cx-AX+cr};
        const float qy[4]={cy-AY+cr,cy+AY-cr,cy+AY-cr,cy-AY+cr};
        const float qa[4]={-(float)M_PI*.5f,0.f,(float)M_PI*.5f,(float)M_PI};
        for(int k=0;k<4;k++)for(int i=0;i<6;i++){
            float t0=qa[k]+(float)M_PI*.5f*i/6,t1=qa[k]+(float)M_PI*.5f*(i+1)/6;
            b.line(qx[k]+cosf(t0)*cr,qy[k]+sinf(t0)*cr,qx[k]+cosf(t1)*cr,qy[k]+sinf(t1)*cr,th,ink);
        }
    };
    auto frame=[&](float k,float th){
        float AX=rx*k,AY=ry*k;
        switch(sy.shape){
            case 1: ell(AX,AY,th,64); break;
            case 2: boxf(AX,AY,0.f,th); break;
            case 3: boxf(AX,AY,std::min(AX,AY)*.28f,th); break;
            case 4: poly(6,AX,AY,-(float)M_PI*.5f,th); break;
            case 5: poly(8,AX,AY,(float)M_PI/8,th); break;
            case 6: {
                float ty=cy-AY+AX;
                b.line(cx-AX,cy+AY,cx+AX,cy+AY,th,ink);
                b.line(cx-AX,cy+AY,cx-AX,ty,th,ink);
                b.line(cx+AX,cy+AY,cx+AX,ty,th,ink);
                for(int i=0;i<24;i++){
                    float a0=(float)M_PI+(float)M_PI*i/24,a1=(float)M_PI+(float)M_PI*(i+1)/24;
                    b.line(cx+cosf(a0)*AX,ty+sinf(a0)*AX,cx+cosf(a1)*AX,ty+sinf(a1)*AX,th,ink);
                }
            } break;
            case 7: {
                b.line(cx-AX,cy-AY,cx+AX,cy-AY,th,ink);
                b.line(cx-AX,cy-AY,cx-AX,cy+AY*.25f,th,ink);
                b.line(cx+AX,cy-AY,cx+AX,cy+AY*.25f,th,ink);
                for(int i=0;i<14;i++){
                    float t0=(float)i/14,t1=(float)(i+1)/14;
                    float x0=cx-AX*(1-t0),y0=cy+AY*.25f+AY*.75f*t0*t0;
                    float x1=cx-AX*(1-t1),y1=cy+AY*.25f+AY*.75f*t1*t1;
                    b.line(x0,y0,x1,y1,th,ink);
                    b.line(2*cx-x0,y0,2*cx-x1,y1,th,ink);
                }
            } break;
            case 8: poly(4,AX,AY,-(float)M_PI*.5f,th); break;
            default: ell(AX,AY,th,72); break;
        }
    };
    frame(1.f,bw);
    if(sy.rings>1)frame(.90f,bw*.55f);
    if(sy.dash){
        float AX=rx*.955f,AY=ry*.955f;
        for(int i=0;i<48;i+=2){
            float a0=2*(float)M_PI*i/48,a1=2*(float)M_PI*(i+.7f)/48;
            b.line(cx+cosf(a0)*AX,cy+sinf(a0)*AY,cx+cosf(a1)*AX,cy+sinf(a1)*AY,bw*.5f,ink);
        }
    }
    if(sy.ticks){
        for(int i=0;i<60;i++){
            float a=2*(float)M_PI*i/60;
            b.line(cx+cosf(a)*rx*.93f,cy+sinf(a)*ry*.93f,
                   cx+cosf(a)*rx*.985f,cy+sinf(a)*ry*.985f,bw*.42f,alpha(ink,150));
        }
    }
    b.flush(ren);

    std::string iso=lowerStr(s.iso);
    if(iso.rfind("n:",0)==0)iso="rnx";
    std::string code=upperStr(iso);
    char serial[32]; snprintf(serial,sizeof serial,"%s %04u",code.c_str(),(unsigned)(s.seed%10000));
    std::string word=d.local.empty()?"ENTRY":d.local;
    { float w,h; stTextSize(F20,word,w,h); if(w<=0.f)word="ENTRY"; }
    std::string line=stPort(s.city,s.iso)+" · "+stDate(s);
    int fw=size>=300?F24:(size>=230?F20:F17), fd=F15;
    int motif=d.motif;
    if(motif<=1){ static const int M[12]={2,3,6,9,11,12,16,19,22,26,30,38}; motif=M[(hs>>21)%12]; }
    SDL_Texture* ov=stOverride(iso);

    if(sy.arc){
        float ax=rx*.80f*sy.inner, ay=ry*.80f*sy.inner;
        std::string ctry=stCountryLabel(s,fw,(ax+ay)*.5f*2.0f*.92f);
        stArcFit(ctry,cx,cy,ax,ay,2.0f,false,fw,ink,ry*.17f);
        stArcFit(std::string(sy.cap)+" · "+code,cx,cy,ax,ay,1.70f,true,fd,alpha(ink,235),ry*.13f);
        if(sy.stars){
            stMotif(b,22,cx+ax,cy,ry*.11f,ink);
            stMotif(b,22,cx-ax,cy,ry*.11f,ink);
        }
        b.flush(ren);
        float BW=rx*1.30f*sy.inner, BH=ry*.62f*sy.inner, BCY=cy+ry*.03f, mh=BH*.30f;
        std::vector<StItem> it;
        it.push_back(StItem{1,"",0,mh,mh});
        { float w,h; stTextSize(fw,word,w,h); it.push_back(StItem{0,word,fw,w,h}); }
        { float w,h; stTextSize(fd,line,w,h); it.push_back(StItem{0,line,fd,w,h}); }
        stStack(it,cx,BCY,BW,BH,size,ink,motif,ov);
    } else {
        float x0=cx-rx*.86f,x1=cx+rx*.86f,y0=cy-ry*.78f,y1=cy+ry*.78f;
        if(sy.shape==6){ y0=cy-ry+rx*.60f; y1=cy+ry*.80f; }
        if(sy.shape==7){ y0=cy-ry*.76f;    y1=cy+ry*.20f; }
        if(sy.shape==8){ x0=cx-rx*.48f; x1=cx+rx*.48f; y0=cy-ry*.48f; y1=cy+ry*.48f; }
        float BW=x1-x0,BH=y1-y0,BCY=(y0+y1)*.5f;
        std::string ctry=stCountryLabel(s,fw,BW*.98f);
        std::vector<StItem> it;
        { float w,h; stTextSize(fw,ctry,w,h); it.push_back(StItem{0,ctry,fw,w,h}); }
        it.push_back(StItem{2,"",0,BW*.8f,std::max(1.f,size*.007f)});
        { float mh=BH*.20f; it.push_back(StItem{1,"",0,mh,mh}); }
        { float w,h; stTextSize(fw,word,w,h); it.push_back(StItem{0,word,fw,w,h}); }
        { float w,h; stTextSize(fd,line,w,h); it.push_back(StItem{0,line,fd,w,h}); }
        { float w,h; stTextSize(fd,serial,w,h); it.push_back(StItem{0,serial,fd,w,h}); }
        stStack(it,cx,BCY,BW,BH,size,ink,motif,ov);
    }
    b.flush(ren);

    std::vector<uint8_t> px((size_t)size*size*4);
    int rp=SDL_RenderReadPixels(ren,nullptr,SDL_PIXELFORMAT_RGBA32,px.data(),size*4);
    SDL_SetRenderTarget(ren,prev);
    SDL_DestroyTexture(tgt);
    if(rp!=0){SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);return nullptr;}
    uint32_t sd=s.seed;
    float dx=((sd%100)/100.f-.5f),dy=(((sd>>5)%100)/100.f-.5f);
    for(int y=0;y<size;y++)for(int x=0;x<size;x++){
        size_t i=((size_t)y*size+x)*4;
        if(px[i+3]==0)continue;
        uint32_t n=sd^(uint32_t)(x*374761393u+y*668265263u);
        n=(n^(n>>13))*1274126177u; n^=n>>16;
        float noise=(n%1000)/1000.f;
        float press=.78f+.22f*clampf(.5f+((x-size*.5f)*dx+(y-size*.5f)*dy)/(size*.5f),0.f,1.f);
        float a=px[i+3]/255.f;
        bool edge=false;
        if(x>0&&px[i-1]==0)edge=true;
        if(x<size-1&&px[i+7]==0)edge=true;
        if(y>0&&px[i-size*4+3]==0)edge=true;
        if(y<size-1&&px[i+size*4+3]==0)edge=true;
        if(noise<(edge?.26f:.10f))a*=.45f+.5f*noise;
        a*=press; if(a>.94f)a=.94f;
        px[i+3]=(Uint8)(a*245);
    }
    SDL_Texture* out=SDL_CreateTexture(ren,SDL_PIXELFORMAT_RGBA32,SDL_TEXTUREACCESS_STATIC,size,size);
    if(out){SDL_UpdateTexture(out,nullptr,px.data(),size*4);SDL_SetTextureBlendMode(out,SDL_BLENDMODE_BLEND);}
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    return out;
}

std::string stKeyOf(const Stamp& s,int size){
    char kb[112];
    snprintf(kb,sizeof kb,"%s|%u|%lld|%d",s.iso.c_str(),(unsigned)s.seed,(long long)s.ts,size);
    return kb;
}
bool stReady(const Stamp& s,int size){ return stTexCache.count(stKeyOf(s,size))>0; }
SDL_Texture* stTex(const Stamp& s,int size){
    std::string k=stKeyOf(s,size);
    auto it=stTexCache.find(k);
    if(it!=stTexCache.end()){ stTexUse[k]=frameNo; return it->second; }
    if(stBakeBudget<=0)return nullptr;
    stBakeBudget--;
    SDL_Texture* tx=stBake(s,size);
    if(!tx)return nullptr;
    stTexCache[k]=tx; stTexUse[k]=frameNo;
    if(stTexCache.size()>40){
        std::string old; Uint32 best=0xFFFFFFFFu;
        for(auto& kv:stTexUse){
            if(kv.second==frameNo||!stTexCache.count(kv.first))continue;
            if(kv.second<best){best=kv.second;old=kv.first;}
        }
        if(!old.empty()){
            auto d=stTexCache.find(old);
            if(d!=stTexCache.end()){SDL_DestroyTexture(d->second);stTexCache.erase(d);}
            stTexUse.erase(old);
        }
    }
    return tx;
}
void stFreeTex(){
    for(auto& kv:stTexCache)if(kv.second)SDL_DestroyTexture(kv.second);
    stTexCache.clear(); stTexUse.clear();
}
void buildVisaKeys(){
    visaKeyBase.clear(); visaIdx.clear();
    for(auto& p:stPlace){
        if(p.idx<0||p.idx>=(int)stamps.v.size())continue;
        visaIdx[p.page].push_back(p.idx);
        visaKeyBase[p.page]+="|"+std::to_string(p.idx)+":"+std::to_string(stamps.v[p.idx].seed);
    }
    visaKeyDirty=false;
}

int stPageCount(){
    int mx=1;
    for(auto& p:stPlace)mx=std::max(mx,p.page);
    int n=std::max(2,mx-1);
    if(n&1)n++;
    return 2+n;
}
int ppSpreadCount(){ return std::max(1,stPageCount()/2); }
void stRebuildLayout(){
    stPlace.clear();
    int page=2;
    std::vector<StPlace> cur;
    auto fit=[&](float x,float y,float r){
        if(x-r<26||x+r>398||y-r<76||y+r>492)return false;
        for(auto& p:cur){float dx=x-p.x,dy=y-p.y,rr=r+p.r;
            if(dx*dx+dy*dy<rr*rr*1.02f)return false;}
        return true;
    };
    for(int i=0;i<(int)stamps.v.size();i++){
        const Stamp& s=stamps.v[i];
        float r=72;
        uint32_t sd=s.seed;
        StPlace p; p.page=page; p.idx=i; p.r=r;
p.rot=((sd%1800)/100.f-9.f)*DEG;
        bool placed=false;
        if(!placed){
            for(int q=0;q<90;q++){
                uint32_t n=sd+q*747796405u; n=(n^(n>>13))*1274126177u; n^=n>>16;
                float x=26+r+(n%1000)/1000.f*(372-2*r);
                float y=76+r+((n>>10)%1000)/1000.f*(416-2*r);
                if(fit(x,y,r)){p.x=x;p.y=y;placed=true;break;}
            }
        }
        if(!placed){page++;cur.clear();p.page=page;
            p.x=26+r+(sd%1000)/1000.f*(372-2*r);
            p.y=76+r+((sd>>10)%1000)/1000.f*(416-2*r);}
        cur.push_back(p); stPlace.push_back(p);
    }
    stLayoutDirty=false;
    for(auto& kv:pgKey)if(kv.first>=2)kv.second.clear();
    visaKeyDirty=true;
}
void stInvalidate(){ stLayoutDirty=true; visaKeyDirty=true; stMapIsoDirty=true; for(auto& kv:pgKey)kv.second.clear(); }
void stAward(int type,const std::string& iso,const std::string& country,const std::string& city,
             const std::string& placeId,const std::string& station,int tzMin,int air){
    if(iso.empty())return;
    std::string key="E:"+iso;
    if(stamps.done.count(key))return;
    stamps.done.insert(key);
    Stamp s; s.iso=iso; s.country=country; s.city=city; s.placeId=placeId; s.station=station;
    s.ts=(long long)time(nullptr); s.tzMin=tzMin; s.type=type; s.airSec=air;
    s.cityHash=pcHash(placeId.empty()?city:placeId);
    s.stationHash=pcHash(station);
    s.seed=pcHash(iso+":"+std::to_string(s.ts)+":"+std::to_string(type)+":"+city);
    s.ink=(int)(s.seed%7);
    stamps.last[iso]=s.ts;
    stamps.v.push_back(std::move(s));
    if(stamps.v.size()>300)stamps.v.erase(stamps.v.begin());
    stSave(stamps);
    stInvalidate();
    stAnimIdx=(int)stamps.v.size()-1; stAnimT=0; stShake=1.f;
    audio.sfx.trigger(Sfx::STAMP);
#if SDL_VERSION_ATLEAST(2,0,9)
    if(pad)SDL_GameControllerRumble(pad,0xB000,0x6000,55);
#endif
    toast(std::string(stTypeName(type))+" STAMP · "+upperStr(country),Pal::warmSoft);
}
void stLeave(){ stCur.clear(); }
void stOnPlay(const Channel& c){
    std::string iso=lowerStr(c.cc.empty()?isoForCountry(c.country):c.cc);
    if(iso.empty())iso="n:"+lowerStr(c.country);
    if(iso!=stCur){ stLeave(); stCur=iso; }
}
void stTick(float dt){
    if(player.state!=Player::PLAYING||!now.valid||stCur.empty())return;
    static float acc=0; acc+=dt;
    if(acc<1.f)return;
    int s=(int)acc; acc-=s;
    stamps.sec[stCur]+=s; stSaveAcc+=s;
    if(stSaveAcc>=30){stSaveAcc=0;stSave(stamps);}
    if(stamps.last.count(stCur))return;
    if(stamps.sec[stCur]<60)return;
    int tz=now.haveTz?now.tzMin:(nowWx.ok?nowWx.tz/60:(int)lroundf(now.lon/15.f)*60);
    stAward(ST_ENTRY,stCur,now.country,now.place.empty()?now.country:now.place,
            now.placeId,now.title,tz,stamps.sec[stCur]);
}
void stAnimUpdate(float dt){
    if(stAnimIdx>=0){
        stAnimT+=dt/.55f;
        if(stAnimT>=1.f){
            stAnimIdx=-1; stAnimT=0;
            stAnimTex=nullptr;
            stAnimTexIdx=-1;
            stInvalidate();
        }
    }
    stShake=std::max(0.f,stShake-dt*2.4f);
}
void stDrawVisa(int idx){
    SDL_Color ink=C(30,36,56),faint=C(120,124,138);
    pcSpaced("VISAS · STAMPS",26,26,F17,1.5f,ink);
    char pg[32]; snprintf(pg,32,"PAGE %d",idx-1);
    txt(F15,pg,398,28,faint,false,2);
    b.line(26,58,398,58,1.4f,alpha(ink,40)); b.flush(ren);
    for(auto& p:stPlace){
        if(p.page!=idx||p.idx==stAnimIdx)continue;
        if(p.idx<0||p.idx>=(int)stamps.v.size())continue;
        SDL_Texture* tx=stTex(stamps.v[p.idx],192);
        if(!tx)continue;
        int d=(int)(p.r*2.1f);
        SDL_Rect dst{(int)(p.x-d*.5f),(int)(p.y-d*.5f),d,d};
        SDL_SetTextureAlphaMod(tx,232);
        SDL_RenderCopyEx(ren,tx,nullptr,&dst,p.rot*180.f/(float)M_PI,nullptr,SDL_FLIP_NONE);
    }
    txt(F15,"AIRWAVES BORDER CONTROL",212,510,alpha(faint,150),false,1);
}
void stMapGlow(){
    if(stMapUntil<=t||homeMode||!globe.places)return;
    if(stMapIsoDirty){
        stMapIso.clear();
        for(auto& stamp:stamps.v)stMapIso.insert(stamp.iso);
        stMapIsoDirty=false;
    }
    if(placeIso.size()!=globe.places->size()){
        placeIso.resize(globe.places->size());
        for(size_t i=0;i<placeIso.size();i++){
            std::string iso=lowerStr(isoForCountry((*globe.places)[i].country));
            placeIso[i]=iso.empty()?"n:"+lowerStr((*globe.places)[i].country):iso;
        }
    }
    for(size_t i=0;i<globe.pp.size()&&i<placeIso.size();i++){
        if(globe.pz[i]<=.05f)continue;
        if(!stMapIso.count(placeIso[i]))continue;
        Vec2 p=globe.pp[i];
        b.ring(p.x,p.y,7,1.4f,alpha(Pal::warm,150),12);
    }
    b.flush(ren);
    txt(F15,"STAMPED COUNTRIES",W/2,70,alpha(Pal::warm,200),true,1);
}
void pageID(){
    SDL_Color ink=C(30,36,56),faint=C(120,124,138),acc=Pal::accent;
    Icon::globe(b,36,34,9,acc); b.flush(ren);
    std::string hd="WORLD PASSPORT";
    float hw=(float)tw(F17,hd,true)+1.5f*(float)(hd.size()-1);
    pcSpaced(hd,52,26,F17,1.5f,ink);
    if(398.f-(52.f+hw+14.f)>70.f)txt(F15,"TYPE P · CODE RNX",398,28,faint,false,2);
    b.line(26,58,398,58,1.4f,alpha(ink,40)); b.flush(ren);
    b.rrect(22,74,158,158,20,C(255,255,255)); b.flush(ren);
    b.line(22,74,180,74,1.6f,alpha(acc,90));
    b.line(22,232,180,232,1.6f,alpha(acc,90));
    b.line(22,74,22,232,1.6f,alpha(acc,90));
    b.line(180,74,180,232,1.6f,alpha(acc,90)); b.flush(ren);
    if(miiTex){ SDL_Rect d{26,78,150,150}; SDL_RenderCopy(ren,miiTex,nullptr,&d); }
    else drawAvatar(26,78,150);
    b.flush(ren);
    drawHologram(176,228); b.flush(ren);
    float nx=198,nw=200;
    txt(F15,"HOLDER",nx,80,faint);
    int nf=tw(F30,pp.nick,true)>(int)nw?F24:F30;
    txt(nf,trunc(nf,pp.nick,(int)nw,true),nx,104,ink,true);
    txt(F15,"NATIONALITY",nx,180,faint);
    Icon::globe(b,nx+9,217,8,acc); b.flush(ren);
    txt(F20,"EARTH",nx+26,204,ink,true);
    txt(F15,"PASSPORT NO.",26,258,faint);
    txt(F20,pp.number,26,276,ink,true);
    txt(F15,"ISSUED",26,312,faint);
    tm it=tmLocal((time_t)pp.issued); char ds[16];
    snprintf(ds,16,"%02d.%02d.%04d",it.tm_mday,it.tm_mon+1,it.tm_year+1900);
    txt(F20,ds,26,330,ink,true);
    txt(F15,"AUTHORITY",226,258,faint);
    txt(F15,"RADIO NX WORLD",226,276,ink,true);
    txt(F15,"SERVICE",226,296,ink,true);
    txt(F15,"EXPIRES",226,326,faint);
    txt(F20,"NEVER",226,344,Pal::warm,true);
    std::string l1,l2; mrzLines(l1,l2);
    b.rrect(12,404,400,88,8,C(236,232,222)); b.flush(ren);
    drawMrzLine(l1,24,418,376,alpha(ink,215));
    drawMrzLine(l2,24,448,376,alpha(ink,215));
    txt(F15,"CITIZEN OF THE AIRWAVES",212,504,alpha(faint,170),false,1);
}
void pageRecord(){
    SDL_Color ink=C(30,36,56),faint=C(120,124,138),acc=Pal::accent;
std::string hd="TRAVEL RECORD";
float hw=(float)tw(F17,hd,true)+1.5f*(float)(hd.size()-1);
pcSpaced(hd,26,26,F17,1.5f,ink);
if(398.f-(26.f+hw+14.f)>70.f)txt(F15,"RADIO NX · AIRWAYS",398,28,faint,false,2);
    b.line(26,58,398,58,1.4f,alpha(ink,40)); b.flush(ren);
    int vals[3]={(int)pp.countries.size(),pp.cities,pp.stations};
    const char* lab[4]={"COUNTRIES VISITED","CITIES TUNED","STATIONS PLAYED","TIME ON AIR"};
    for(int i=0;i<4;i++){
        float ry=76+i*62.f;
        b.circle(44,ry+26,20,alpha(acc,20),18); b.flush(ren);
        if(i==0)Icon::globe(b,44,ry+26,10,acc);
        else if(i==1)PcIcon::card(b,44,ry+26,9,acc);
        else if(i==2)HomeIcon::radio(b,44,ry+26,9,acc,acc);
        else Icon::clock(b,44,ry+26,10,acc);
        b.flush(ren);
        if(i<3)txt(F30,std::to_string(vals[i]),78,ry+6,ink,true);
        else { char sb[24]; long long s=pp.airSec;
            snprintf(sb,24,"%lldh %02lldm",(long long)(s/3600),(long long)((s%3600)/60));
            txt(F24,sb,78,ry+10,ink,true); }
        txt(F15,lab[i],78,ry+40,faint);
        b.line(78,ry+56,398,ry+56,1,alpha(ink,18)); b.flush(ren);
    }
    txt(F15,"VISITED FLAGS",26,336,faint);
    if(pp.countries.empty()){
        Icon::globe(b,42,380,10,alpha(faint,150)); b.flush(ren);
        txt(F15,"Tune into a city to start your record",62,372,faint);
    } else {
        int sh=0;
        for(size_t i=0;i<pp.countries.size()&&sh<12;i++){
            const std::string& ck=pp.countries[i];
            float fx=26+(sh%6)*58.f, fy=358+(sh/6)*44.f;
            if(ck.rfind("n:",0)==0){
                b.rrect(fx,fy,44,30,5,alpha(ink,20)); b.flush(ren);
                txt(F15,upperStr(ck.substr(2,2)),fx+22,fy+7,alpha(ink,200),true,1);
            }
            sh++;
        }
        if(pp.countries.size()>12){ char m[16]; snprintf(m,16,"+%d",(int)pp.countries.size()-12);
            txt(F17,m,26+6*58.f-14,408,faint,true); }
    }
    txt(F15,"This document certifies the holder",26,470,faint);
    txt(F15,"as a citizen of the airwaves.",26,490,faint);
    Icon::globe(b,384,482,9,alpha(acc,120)); b.flush(ren);
}

void drawRecordFlagsLive(float px,float py,float a){
    if(a<.98f)return;
    int sh=0;
    for(size_t i=0;i<pp.countries.size()&&sh<12;i++){
        const std::string& ck=pp.countries[i];
        if(ck.rfind("n:",0)!=0)
            drawFlag(px+26+(sh%6)*58.f, py+358+(sh/6)*44.f, 44,30, ck, "");
        sh++;
    }
}

void ensurePage(int idx){
    if(idx<0||idx>=stPageCount())return;
    if(stLayoutDirty)stRebuildLayout();
    std::string key;
    int visaReady=0,visaTotal=0;
    if(idx==0)key="id|"+pp.nick+"|"+pp.number+"|"+std::to_string(pp.issued)+"|"+(miiTex?"m":"d")+"|"+std::to_string((int)ppSeed)+"|"+std::to_string(stamps.v.size());
    else if(idx==1)key="rec|"+std::to_string(pp.countries.size())+"|"+std::to_string(pp.cities)+"|"+std::to_string(pp.stations)+"|"+std::to_string(pp.airSec/60)+"|"+std::to_string(stamps.v.size());
    else{
        if(visaKeyDirty)buildVisaKeys();
        auto vi=visaIdx.find(idx);
        if(vi!=visaIdx.end()){visaTotal=(int)vi->second.size();for(int id:vi->second)if(stReady(stamps.v[id],192))visaReady++;}
        key="visa|"+std::to_string(idx)+"|"+std::to_string(stamps.v.size())+"|"+
            std::to_string(stAnimIdx)+"|"+std::to_string(visaReady)+"|"+visaKeyBase[idx];
    }
    auto it=pgTex.find(idx);
    if(it!=pgTex.end()&&it->second&&pgKey[idx]==key&&(idx<2||visaReady>=visaTotal)){pgUse[idx]=frameNo;return;}
    SDL_Texture* tx=it!=pgTex.end()?it->second:nullptr;
    if(!tx){
        tx=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,424,536);
        if(!tx)return;
        SDL_SetTextureBlendMode(tx,SDL_BLENDMODE_BLEND);pgTex[idx]=tx;
    }
    b.flush(ren);
    SDL_RenderSetClipRect(ren,nullptr);
    SDL_SetRenderTarget(ren,tx);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,0,0,0,0);SDL_RenderClear(ren);
    b.rrect2(0,0,424,536,10,10,C(249,247,241));
    b.gradRect(2,2,420,90,alpha(C(255,255,255),50),alpha(C(255,255,255),0));
    b.flush(ren);
    if(idx==0)pageID();
    else if(idx==1)pageRecord();
    else stDrawVisa(idx);
    b.flush(ren);
    SDL_SetRenderTarget(ren,nullptr);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    pgKey[idx]=key; pgUse[idx]=frameNo;
    if(pgTex.size()>8){
        int victim=-1; Uint32 best=0xFFFFFFFFu;
        for(auto& kv:pgUse){
            if(kv.second==frameNo||!pgTex.count(kv.first))continue;
            if(kv.second<best){best=kv.second;victim=kv.first;}
        }
        if(victim>=0){
            auto d=pgTex.find(victim);
            if(d!=pgTex.end()){if(d->second)SDL_DestroyTexture(d->second);pgTex.erase(d);}
            pgKey.erase(victim);pgUse.erase(victim);
        }
    }
}

void drawPassport(){
    if(passportMix<.01f)return;
    stBakeBudget=ppTurning?0:1;
    if(stLayoutDirty)stRebuildLayout();
    const float pw=424,ph=536;
    float A=easeOut(passportMix);
    float op=easeOut(clampf((passportMix-.05f)/.95f,0.f,1.f));
    float shake=stShake*sinf(t*42.f)*2.4f;
    float bob=sinf(t*.8f)*2.f*op+shake;
    float y0=92+bob,y1=y0+ph;
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,6,8,14,(Uint8)(238*A));
    SDL_Rect full{0,0,W,H};SDL_RenderFillRect(ren,&full);
    bookRect={196,70,888,580};
    bool opened=(op>.995f);
    float spine=opened?640.f:lerpf(640.f-pw*.5f,640.f,op);
    auto hgrad=[&](float x,float w,float y,float h,SDL_Color cl,SDL_Color cr){
        if(w<=0)return;
        int p0=b.vert(x,y,cl),p1=b.vert(x+w,y,cr),p2=b.vert(x+w,y+h,cr),p3=b.vert(x,y+h,cl);
        b.tri(p0,p1,p2);b.tri(p0,p2,p3);
    };
    auto flat=[&](int idx,float px,float a){
        if(a<=.01f)return;
        ensurePage(idx);
        auto it=pgTex.find(idx);
        if(it==pgTex.end()||!it->second)return;
        SDL_Rect d{(int)px,(int)y0,(int)pw,(int)ph};
        SDL_SetTextureAlphaMod(it->second,(Uint8)(255*clampf(a,0.f,1.f)));
        SDL_RenderCopy(ren,it->second,nullptr,&d);
    };
    auto squash=[&](int idx,float x,float w,float a,float sth){
        if(a<=.01f||w<1.f)return;
        ensurePage(idx);
        auto it=pgTex.find(idx);
        if(it==pgTex.end()||!it->second)return;
        float lift=ph*.045f*sth;
        SDL_Rect d{(int)x,(int)(y0-lift*.5f),(int)w,(int)(ph+lift)};
        SDL_SetTextureAlphaMod(it->second,(Uint8)(255*clampf(a,0.f,1.f)));
        SDL_RenderCopy(ren,it->second,nullptr,&d);
        hgrad(d.x,d.w,d.y,d.h,alpha(C(0,0,0),(int)(70*sth*a)),alpha(C(0,0,0),0));
        b.flush(ren);
    };
    float bcA=opened?1.f:clampf((op-.35f)/.4f,0.f,1.f);
    if(bcA>.01f){
        float bx=spine-pw-16,bw=2*pw+32;
        pcShadow(b,spine,y1+24,bw*.46f,15,alpha(C(0,0,0),(int)(150*A*bcA)),26);b.flush(ren);
        b.rrect2(bx,y0-16,bw,ph+32,24,20,alpha(C(23,30,58),(int)(255*A*bcA)));
        b.line(bx+18,y0-14,bx+bw-18,y0-14,1.4f,alpha(C(232,198,124),(int)(60*A*bcA)));
        b.flush(ren);
    }
    if(opened){
        int s=ppSpread;
        if(ppTurning){
            float u=clampf(ppTurn,0.f,1.f);
            float th=(ppDir>0)?(float)M_PI*u:(float)M_PI*(1.f-u);
            float half=pw*cosf(th),sth=sinf(th);
            int stayL=(ppDir>0)?2*s:2*s-2;
            int stayR=(ppDir>0)?2*s+3:2*s+1;
            int faceR=(ppDir>0)?2*s+1:2*s-1;
            int faceL=(ppDir>0)?2*s+2:2*s;
            flat(stayL,spine-pw,A);flat(stayR,spine,A);
            float ex=spine+half,dir=(half>=0)?1.f:-1.f;
            if(dir>0)hgrad(ex,34*sth,y0,ph,alpha(C(0,0,0),(int)(80*sth*A)),alpha(C(0,0,0),0));
            else hgrad(ex-34*sth,34*sth,y0,ph,alpha(C(0,0,0),0),alpha(C(0,0,0),(int)(80*sth*A)));
            b.flush(ren);
            if(half>=0)squash(faceR,spine,half,A,sth);
            else squash(faceL,spine+half,-half,A,sth);
        }else{
            flat(2*s,spine-pw,A);flat(2*s+1,spine,A);
            if(s==0){ drawAvatarLive(spine-pw+26,y0+78,150,A); drawRecordFlagsLive(spine,y0,A); }
            if(stAnimIdx>=0){
                for(auto& p:stPlace){
                    if(p.idx!=stAnimIdx)continue;
                    int pg=p.page;
                    if(pg!=2*s&&pg!=2*s+1)continue;
                    float px=(pg==2*s)?spine-pw:spine;
                    if(stAnimTexIdx!=p.idx){
                        SDL_Texture* t2=stTex(stamps.v[p.idx],256);
                        if(t2){stAnimTex=t2;stAnimTexIdx=p.idx;}
                    }
                    SDL_Texture* tx=stAnimTex;
                    if(tx){
                        float u=easeOut(clampf(stAnimT,0.f,1.f));
                        float sc=1.6f-.6f*u;
                        int d=(int)(p.r*2.1f*sc);
                        SDL_Rect dst{(int)(px+p.x-d*.5f),(int)(y0+p.y-d*.5f-(1.f-u)*90.f),d,d};
                        SDL_SetTextureAlphaMod(tx,(Uint8)(230*u));
                        SDL_RenderCopyEx(ren,tx,nullptr,&dst,(p.rot+(1.f-u)*.05f)*180.f/(float)M_PI,nullptr,SDL_FLIP_NONE);
                        for(int k=0;k<8;k++){
                            float a=k*.785f+stAnimT*2.f;
                            float rr=p.r*(.6f+stAnimT*.8f);
                            b.circle(px+p.x+cosf(a)*rr,y0+p.y+sinf(a)*rr,1.6f,alpha(Pal::warm,(int)(160*(1.f-u))),6);
                        }
                        b.flush(ren);
                    }
                }
            }
        }
        b.quad(spine-12,y0-16,spine+12,y0-16,spine+12,y1+16,spine-12,y1+16,alpha(C(0,0,0),(int)(55*A)));
        b.line(spine,y0-8,spine,y1+8,1.4f,alpha(C(0,0,0),(int)(90*A)));
        b.flush(ren);
    }else{
        float th=op*(float)M_PI,half=pw*cosf(th),sth=sinf(th);
        bool had0=pgTex.count(0)>0;
        flat(0,spine-pw,clampf((op-.45f)/.3f,0.f,1.f)*A);
        if(had0){
            float rl=spine+std::max(0.f,half);
            if(spine+pw-rl>2.f){
                SDL_Rect clip{(int)rl,(int)y0,(int)(spine+pw-rl),(int)ph};
                SDL_RenderSetClipRect(ren,&clip);
                flat(1,spine,A);
                SDL_RenderSetClipRect(ren,nullptr);
            }
        }
        float ca=A*(1.f-clampf((op-.72f)/.24f,0.f,1.f));
        if(ca>.01f){
            float lift=ph*.05f*sth;
            if(half>=0){
                buildCover();
                if(coverTex){
                    float bl=12*(half/pw);
                    SDL_Rect d{(int)(spine-bl*.5f),(int)(y0-bl*.5f-lift*.5f),
                               (int)std::max(2.f,half+bl),(int)(ph+bl+lift)};
                    SDL_SetTextureAlphaMod(coverTex,(Uint8)(255*ca));
                    SDL_RenderCopy(ren,coverTex,nullptr,&d);
                    hgrad(d.x,d.w,d.y,d.h,alpha(C(0,0,0),(int)(60*sth*ca)),alpha(C(0,0,0),0));
                    b.flush(ren);
                }
            }else{
                float w=-half,lift2=ph*.05f*sth;
                b.rrect2(spine+half,y0-6-lift2*.5f,w,ph+12+lift2,16,14,alpha(C(36,46,84),(int)(255*ca)));
                b.flush(ren);
            }
        }
    }
}

    void pcTitle(const std::string& s,float x,float y,int fi,int maxW,SDL_Color ink,SDL_Color sh,float phase){
        auto measure=[&](int ff,float tr,const std::string& str){
            float w=0; size_t i=0;
            while(i<str.size()){ size_t st=i; utf8Next(str,i); w+=tw(ff,str.substr(st,i-st),true)+tr; }
            return w>0?w-tr:0.f;
        };
        int f=fi; float track=3.f;
        while(f>F15&&measure(f,track,s)>(float)maxW){ track-=1.f; if(track<0.f){track=2.f;f--;} }
        std::string out=s;
        if(measure(f,track,out)>(float)maxW){ track=0.f; out=trunc(f,s,maxW,true); }
        float cx=x; size_t i=0; int k=0;
        while(i<out.size()){
            size_t st=i; utf8Next(out,i); std::string ch=out.substr(st,i-st);
            float dy=sinf(phase+k*.55f)*3.2f;
            txt(f,ch,(int)(cx+2),(int)(y+dy+3),sh,true);
            txt(f,ch,(int)cx,(int)(y+dy),ink,true);
            cx+=tw(f,ch,true)+track; k++;
        }
    }
    void pcSpaced(const std::string& s,float x,float y,int fi,float tr,SDL_Color c){
        float cx=x; size_t i=0;
        while(i<s.size()){ size_t st=i; utf8Next(s,i); std::string ch=s.substr(st,i-st);
            txt(fi,ch,(int)cx,(int)y,c,true); cx+=tw(fi,ch,true)+tr; }
    }

    void pcArt(const SDL_Rect& r,const PcTheme& th,const Card& c){
        SDL_RenderSetClipRect(ren,&r);
        uint32_t hh=pcHash(c.id);
        int scene=(int)(hh%3);
        float RW=(float)r.w,RH=(float)r.h,RX=(float)r.x,RY=(float)r.y;
        SDL_Color sky1=mixc(th.a2,C(255,255,255),.55f);
        SDL_Color sky2=mixc(th.paper,th.a1,.10f);
        b.gradRect(RX,RY,RW,RH,alpha(sky1,255),alpha(sky2,255));

        float sx=RX+RW*(.20f+(hh%45)/100.f),sy=RY+RH*.24f;
        b.gradCircle(sx,sy,RH*.20f,alpha(mixc(th.a2,C(255,255,255),.6f),120),alpha(th.a2,0),22);
        b.circle(sx,sy,RH*.055f,alpha(mixc(C(255,252,240),th.a2,.25f),245),18);

        for(int i=0;i<3;i++){
            float cx=RX+RW*(.15f+((hh>>(i*3))%70)/100.f),cy=RY+RH*(.14f+.10f*i),s=RH*(.035f+.012f*i);
            SDL_Color cc2=alpha(C(255,255,255),150-i*25);
            b.circle(cx,cy,s,cc2,12); b.circle(cx+s*.9f,cy+s*.15f,s*.8f,cc2,12);
            b.circle(cx-s*.9f,cy+s*.2f,s*.7f,cc2,12);
            b.quad(cx-s*1.6f,cy,cx+s*1.6f,cy,cx+s*1.6f,cy+s*.7f,cx-s*1.6f,cy+s*.7f,cc2);
        }
        if(scene==0){
            float base=RY+RH*.66f;
            for(int k=0;k<3;k++){
                SDL_Color col=alpha(mixc(th.a1,C(255,255,255),.35f-.16f*k),255);
                float amp=RH*(.20f-.05f*k),off=base-RH*.02f*k;
                const int N=10;
                for(int i=0;i<N;i++){
                    float x0=RX+RW*i/N,x1=RX+RW*(i+1)/N,xm=(x0+x1)*.5f;
                    float pk=off-amp*(.55f+.45f*sinf(i*1.7f+k*2.1f+(hh%9)));
                    b.tri(b.vert(x0,off,col),b.vert(x1,off,col),b.vert(xm,pk,col));
                    if(k==0)b.tri(b.vert(xm-(x1-x0)*.16f,pk+amp*.20f,alpha(C(255,255,255),210)),
                                  b.vert(xm+(x1-x0)*.16f,pk+amp*.20f,alpha(C(255,255,255),210)),
                                  b.vert(xm,pk,alpha(C(255,255,255),230)));
                }
                b.quad(RX,off,RX+RW,off,RX+RW,RY+RH,RX,RY+RH,col);
            }
            b.quad(RX,base,RX+RW,base,RX+RW,RY+RH,RX,RY+RH,alpha(mixc(th.a2,C(255,255,255),.35f),255));
            for(int i=0;i<7;i++){
                float yy=base+RH*.035f*(i+1),ww=RW*(.30f+.10f*((hh>>i)%5));
                float xx=RX+RW*.5f-ww*.5f+RW*.12f*sinf(i*1.3f);
                b.line(xx,yy,xx+ww,yy,2.f,alpha(C(255,255,255),110-i*10));
            }
        } else if(scene==1){
            float base=RY+RH*.74f,bw=RW/13.f;
            for(int i=0;i<13;i++){
                uint32_t v=pcHash(c.id+std::to_string(i*7));
                float bh=RH*(.14f+(v%45)/100.f*.36f),x=RX+i*bw;
                SDL_Color bc2=alpha(mixc(th.a1,C(20,24,34),.20f+.03f*(i%4)),255);
                b.quad(x+1,base-bh,x+bw-1,base-bh,x+bw-1,base,x+1,base,bc2);
                if(v%5==0)b.line(x+bw*.5f,base-bh,x+bw*.5f,base-bh-RH*.06f,2.f,bc2);
                for(int wy=0;wy<4;wy++)for(int wx=0;wx<2;wx++)if((v>>(wy*2+wx))&1){
                    float wx0=x+bw*.24f+wx*bw*.36f,wy0=base-bh+RH*.035f+wy*RH*.05f;
                    if(wy0<base-4)b.quad(wx0,wy0,wx0+bw*.18f,wy0,wx0+bw*.18f,wy0+RH*.022f,wx0,wy0+RH*.022f,
                                         alpha(mixc(th.a2,C(255,240,200),.6f),200));
                }
            }
            b.quad(RX,base,RX+RW,base,RX+RW,RY+RH,RX,RY+RH,alpha(mixc(th.a2,C(30,40,60),.35f),255));
            for(int i=0;i<6;i++){
                float yy=base+RH*.04f*(i+1);
                b.line(RX+RW*.08f,yy,RX+RW*.92f,yy,1.6f,alpha(C(255,255,255),70-i*8));
            }
        } else {
            for(int k=0;k<3;k++){
                float base=RY+RH*(.58f+.13f*k),amp=RH*(.09f-.02f*k);
                SDL_Color col=alpha(mixc(th.a1,C(255,255,255),.30f-.14f*k),255);
                const int N=20;
                for(int i=0;i<N;i++){
                    float x0=RX+RW*i/N,x1=RX+RW*(i+1)/N;
                    float y0=base-sinf(i*.55f+k*1.4f+(hh%7))*amp,y1=base-sinf((i+1)*.55f+k*1.4f+(hh%7))*amp;
                    b.quad(x0,y0,x1,y1,x1,RY+RH,x0,RY+RH,col);
                }
            }
            for(int i=0;i<4;i++){
                float bx=RX+RW*(.55f+.09f*i),by=RY+RH*(.20f+.05f*((hh>>i)%4)),s=RW*.018f;
                SDL_Color bc2=alpha(th.ink,140);
                b.line(bx-s,by,bx,by-s*.55f,1.6f,bc2); b.line(bx,by-s*.55f,bx+s,by,1.6f,bc2);
            }
        }
        b.flush(ren);
        SDL_RenderSetClipRect(ren,nullptr);
    }
void pcBuildFace(const Card& c){
    const int PAD=22;
    PcLay L=pcLayout(c);

    if(!pcFace){
        pcFace=SDL_CreateTexture(ren,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_TARGET,680,640);
        if(pcFace)SDL_SetTextureBlendMode(pcFace,SDL_BLENDMODE_BLEND);
    }
    if(!pcFace)return;

    pcFaceW=(int)L.w+PAD*2;
    pcFaceH=(int)L.h+PAD*2;

    PcTheme th=pcTheme(c);

    b.flush(ren);
    SDL_SetRenderTarget(ren,pcFace);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,0,0,0,0);
    SDL_RenderClear(ren);

    float ox=PAD,oy=PAD;
    int v=c.variant%12;
    float rad=(v==1||v==5||v==8)?24.f:(v==2||v==6)?5.f:(v==9)?8.f:14.f;

    SDL_Rect clip{(int)ox,(int)oy,(int)L.w,(int)L.h};
    SDL_RenderSetClipRect(ren,&clip);

    b.rrect2(ox,oy,L.w,L.h,rad,rad*.8f,th.paper);
    b.gradRect(ox+2,oy+2,L.w-4,L.h*.45f,alpha(C(255,255,255),46),alpha(C(255,255,255),0));

    uint32_t hh=pcHash(c.id);

    auto dash=[&](float x0,float y0,float x1,float y1,float seg,float gap,float thick,SDL_Color col){
        float dx=x1-x0,dy=y1-y0,len=sqrtf(dx*dx+dy*dy);
        if(len<1)return;
        dx/=len;
        dy/=len;
        for(float p=0;p<len;p+=seg+gap){
            float e=std::min(p+seg,len);
            b.line(x0+dx*p,y0+dy*p,x0+dx*e,y0+dy*e,thick,col);
        }
    };

    if(v==0){
        b.rrect2(ox+8,oy+8,L.w-16,L.h-16,rad-4,rad-6,alpha(th.a1,170));
        b.rrect2(ox+13,oy+13,L.w-26,L.h-26,rad-7,rad-9,alpha(th.a2,120));
    }
    else if(v==1){
        int kx=0;
        for(float x=ox;x<=ox+L.w;x+=13.f,++kx){
            b.circle(x,oy,7.f,alpha((kx&1)?th.a2:th.a1,140),10);
            b.circle(x,oy+L.h,7.f,alpha((kx&1)?th.a1:th.a2,140),10);
        }

        int ky=0;
        for(float y=oy;y<=oy+L.h;y+=13.f,++ky){
            b.circle(ox,y,7.f,alpha((ky&1)?th.a2:th.a1,130),10);
            b.circle(ox+L.w,y,7.f,alpha((ky&1)?th.a1:th.a2,130),10);
        }

        b.rrect2(ox+12,oy+12,L.w-24,L.h-24,rad-9,rad-11,alpha(th.ink,28));
    }
    else if(v==2){
        for(float x=ox;x<=ox+L.w;x+=11.f){
            b.circle(x,oy,4.4f,alpha(th.a1,130),8);
            b.circle(x,oy+L.h,4.4f,alpha(th.a1,130),8);
        }
        for(float y=oy;y<=oy+L.h;y+=11.f){
            b.circle(ox,y,4.4f,alpha(th.a2,120),8);
            b.circle(ox+L.w,y,4.4f,alpha(th.a2,120),8);
        }
        b.rrect2(ox+12,oy+12,L.w-24,L.h-24,3,3,alpha(th.ink,32));
    }
    else if(v==3){
        b.rrect2(ox+7,oy+7,L.w-14,L.h-14,rad-3,rad-5,alpha(th.ink,24));
        b.flush(ren);

        auto rib=[&](float cx,float cy,SDL_Color col){
            float ca=cosf(-.785f),sa=sinf(-.785f),hw=66,h2=10;
            b.quad(cx-ca*hw+sa*h2,cy-sa*hw-ca*h2,
                   cx+ca*hw+sa*h2,cy+sa*hw-ca*h2,
                   cx+ca*hw-sa*h2,cy+sa*hw+ca*h2,
                   cx-ca*hw-sa*h2,cy-sa*hw+ca*h2,
                   col);
        };

        rib(ox+28,oy+28,alpha(th.a1,220));
        rib(ox+L.w-28,oy+L.h-28,alpha(th.a2,200));
    }
    else if(v==4){
        b.rrect2(ox+9,oy+9,L.w-18,L.h-18,3,3,alpha(th.a1,140));
        b.flush(ren);

        for(int k=0;k<4;k++){
            float cx=(k&1)?ox+L.w-14:ox+14;
            float cy=(k&2)?oy+L.h-14:oy+14;
            float dx=(k&1)?-1.f:1.f;
            float dy=(k&2)?-1.f:1.f;

            b.line(cx,cy,cx+dx*28,cy,3.f,alpha(th.a2,230));
            b.line(cx,cy,cx,cy+dy*28,3.f,alpha(th.a2,230));
            b.line(cx+dx*7,cy+dy*7,cx+dx*22,cy+dy*7,1.4f,alpha(th.a1,180));
            b.line(cx+dx*7,cy+dy*7,cx+dx*7,cy+dy*22,1.4f,alpha(th.a1,180));
            b.circle(cx+dx*34,cy+dy*34,2.6f,alpha(th.a3,190),7);
        }
    }
    else if(v==5){
        b.rrect2(ox+6,oy+6,L.w-12,L.h-12,rad-4,rad-6,alpha(th.a1,190));
        b.flush(ren);

        for(int i=0;i<30;i++){
            float x=ox+((hh>>(i%13))%991)/991.f*L.w;
            float y=oy+((hh>>(i%9))%983)/983.f*L.h;
            SDL_Color cc2=alpha(i%3==0?th.a1:(i%3==1?th.a2:th.a3),100+(i%3)*40);

            if(i%3==0)b.circle(x,y,2.8f,cc2,7);
            else b.tri(b.vert(x,y-3.4f,cc2),b.vert(x+3.1f,y+2.5f,cc2),b.vert(x-3.1f,y+2.5f,cc2));
        }
    }
    else if(v==6){
        for(int k=0;k<3;k++){
            float xx=ox+5+k*4.6f;
            SDL_Color col=k==0?th.a1:(k==1?th.a3:th.a2);
            b.quad(xx,oy+6,xx+3.2f,oy+6,xx+3.2f,oy+L.h-6,xx,oy+L.h-6,alpha(col,215));
        }
        b.rrect2(ox+23,oy+8,L.w-31,L.h-16,4,4,alpha(th.ink,22));
    }
    else if(v==7){
        b.rrect2(ox+8,oy+8,L.w-16,L.h-16,rad-4,rad-6,alpha(th.ink,26));
        b.flush(ren);

        for(int i=0;i<28;i++){
            float a=i*(float)M_PI/14;
            b.line(ox+L.w*.5f+cosf(a)*(L.w*.44f),
                   oy+L.h*.5f+sinf(a)*(L.h*.44f),
                   ox+L.w*.5f+cosf(a)*(L.w*.47f),
                   oy+L.h*.5f+sinf(a)*(L.h*.47f),
                   1.7f,alpha(th.a1,85));
        }

        dash(ox+16,oy+16,ox+L.w-16,oy+16,7,5,1.6f,alpha(th.a2,150));
        dash(ox+16,oy+L.h-16,ox+L.w-16,oy+L.h-16,7,5,1.6f,alpha(th.a2,150));
        dash(ox+16,oy+16,ox+16,oy+L.h-16,7,5,1.6f,alpha(th.a2,150));
        dash(ox+L.w-16,oy+16,ox+L.w-16,oy+L.h-16,7,5,1.6f,alpha(th.a2,150));
    }
    else if(v==8){
        b.rrect2(ox+7,oy+7,L.w-14,L.h-14,rad-4,rad-6,alpha(th.a1,165));
        b.rrect2(ox+11,oy+11,L.w-22,L.h-22,rad-7,rad-9,alpha(th.a2,70));
        b.flush(ren);

        for(int k=0;k<5;k++)
            PcIcon::leaf(b,ox+26+k*20,oy+L.h-24,8,alpha(k%2?th.a2:th.a1,190));

        PcIcon::leaf(b,ox+L.w-28,oy+30,11,alpha(th.a1,200));
        PcIcon::leaf(b,ox+L.w-46,oy+44,7,alpha(th.a2,150));
    }
    else if(v==9){
        for(float x=ox+16;x<ox+L.w;x+=19)
            b.line(x,oy,x,oy+L.h,1,alpha(th.ink,13));
        for(float y=oy+16;y<oy+L.h;y+=19)
            b.line(ox,y,ox+L.w,y,1,alpha(th.ink,13));

        b.flush(ren);

        auto tape=[&](float cx,float cy,float ang,SDL_Color col){
            float ca=cosf(ang),sa=sinf(ang),hw=46,h2=14;
            b.quad(cx-ca*hw+sa*h2,cy-sa*hw-ca*h2,
                   cx+ca*hw+sa*h2,cy+sa*hw-ca*h2,
                   cx+ca*hw-sa*h2,cy+sa*hw+ca*h2,
                   cx-ca*hw-sa*h2,cy-sa*hw+ca*h2,
                   col);
        };

        tape(ox+20,oy+18,-.6f,th.tape);
        tape(ox+L.w-20,oy+L.h-18,-.6f,alpha(mixc(th.a1,C(255,255,255),.45f),150));
        b.rrect2(ox+10,oy+10,L.w-20,L.h-20,4,4,alpha(th.ink,20));
    }
    else if(v==10){
        float cx=ox+L.w-34,cy=oy+34;

        for(int i=0;i<18;i++){
            float a=i*(float)M_PI/9+.15f;
            b.line(cx,cy,cx+cosf(a)*300,cy+sinf(a)*300,8,alpha(th.a2,30));
        }

        b.rrect2(ox+6,oy+6,L.w-12,L.h-12,rad-4,rad-6,alpha(th.a1,180));
        b.flush(ren);

        for(int i=0;i<16;i++){
            float x=ox+((hh>>(i%11))%977)/977.f*L.w;
            float y=oy+((hh>>(i%7))%971)/971.f*L.h;
            b.circle(x,y,2.4f,alpha(th.a3,110),7);
        }
    }
    else {
        b.rrect2(ox+9,oy+9,L.w-18,L.h-18,rad-5,rad-7,alpha(th.a2,60));
        b.flush(ren);

        dash(ox+14,oy+14,ox+L.w-14,oy+14,9,6,2.2f,alpha(th.a1,200));
        dash(ox+14,oy+L.h-14,ox+L.w-14,oy+L.h-14,9,6,2.2f,alpha(th.a1,200));
        dash(ox+14,oy+14,ox+14,oy+L.h-14,9,6,2.2f,alpha(th.a1,200));
        dash(ox+L.w-14,oy+14,ox+L.w-14,oy+L.h-14,9,6,2.2f,alpha(th.a1,200));

        for(int k=0;k<4;k++){
            float cx=(k&1)?ox+L.w-14:ox+14;
            float cy=(k&2)?oy+L.h-14:oy+14;
            b.circle(cx,cy,6.f,alpha(th.a2,220),12);
            b.line(cx-3,cy,cx+3,cy,1.2f,alpha(th.a3,220));
            b.line(cx,cy-3,cx,cy+3,1.2f,alpha(th.a3,220));
        }
    }

    b.flush(ren);

    SDL_Rect pr{
        (int)(ox+L.photo.x),
        (int)(oy+L.photo.y),
        L.photo.w,
        L.photo.h
    };

if(pcPhoto.tex){
    int iw=pcPhoto.w,ih=pcPhoto.h;
        float ta=pr.w/(float)pr.h;
        float sa=iw/(float)ih;
        SDL_Rect src;

        if(sa>ta){
            src.h=ih;
            src.w=(int)(ih*ta+.5f);
            src.x=(iw-src.w)/2;
            src.y=0;
        } else {
            src.w=iw;
            src.h=(int)(iw/ta+.5f);
            src.x=0;
            src.y=(ih-src.h)/2;
        }

        SDL_RenderCopy(ren,pcPhoto.tex,&src,&pr);
    } else {
        pcArt(pr,th,c);
    }

    b.line((float)pr.x,(float)pr.y,(float)(pr.x+pr.w),(float)pr.y,2,alpha(th.ink,80));
    b.line((float)pr.x,(float)(pr.y+pr.h),(float)(pr.x+pr.w),(float)(pr.y+pr.h),2,alpha(th.ink,80));
    b.line((float)pr.x,(float)pr.y,(float)pr.x,(float)(pr.y+pr.h),2,alpha(th.ink,80));
    b.line((float)(pr.x+pr.w),(float)pr.y,(float)(pr.x+pr.w),(float)(pr.y+pr.h),2,alpha(th.ink,80));
    b.flush(ren);

    float sx=ox+L.stampX;
    float sy=oy+L.stampY;
    float sw=L.stampW;
    float sh=L.stampH;

    b.rrect2(sx-3,sy-3,sw+6,sh+6,3,3,alpha(th.ink,26));
    b.rrect2(sx,sy,sw,sh,2,2,alpha(C(255,255,255),240));

    for(float x=sx;x<=sx+sw;x+=7.f){
        b.circle(x,sy,2.6f,th.paper,6);
        b.circle(x,sy+sh,2.6f,th.paper,6);
    }
    for(float y=sy;y<=sy+sh;y+=7.f){
        b.circle(sx,y,2.6f,th.paper,6);
        b.circle(sx+sw,y,2.6f,th.paper,6);
    }

    b.flush(ren);

    drawFlag(sx+6,sy+7,sw-12,sh-26,c.cc,c.country);
    txt(F15,"NX",(int)(sx+sw/2),(int)(sy+sh-18),alpha(th.ink,200),true,1);

    tm dt=tmLocal((time_t)c.ts);
    char ds[16];
    snprintf(ds,16,"%02d.%02d.%02d",dt.tm_mday,dt.tm_mon+1,(dt.tm_year+1900)%100);

    float mx=sx-6;
    float my=sy+sh-4;

    b.ring(mx,my,23,1.8f,alpha(th.at,165),26);
    b.ring(mx,my,18,1.2f,alpha(th.at,110),24);
    b.flush(ren);

    txt(F15,ds,(int)mx,(int)my-8,alpha(th.at,215),true,1);

    float tx=ox+L.tx;

    pcTitle(c.city,
            tx,
            oy+L.nameY,
            L.nf,
            (int)L.twid,
            th.ink,
            alpha(th.ink,45),
            (float)(hh%628)/100.f);

    pcSpaced(upperStr(c.country),tx,oy+L.ctryY,15,2.4f,alpha(th.at,225));

    float uw=std::min((float)tw(F15,upperStr(c.country),true)+2.4f*(float)c.country.size(),L.twid);

    b.line(tx,oy+L.ctryY+24,tx+uw,oy+L.ctryY+24,2,alpha(th.a2,190));
    b.circle(tx+uw+8,oy+L.ctryY+24,3,alpha(th.at,190),8);
    b.flush(ren);

    SDL_RenderSetClipRect(ren,nullptr);
    SDL_SetRenderTarget(ren,nullptr);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
}

void drawPostcard(){
    if(pcMix<.01f||!pcCurValid)return;
    const Card& c=pcCur;
    float e=easeOut(pcMix);
    SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren,4,6,12,(Uint8)(205*e));
    SDL_Rect full{0,0,W,H}; SDL_RenderFillRect(ren,&full);
    std::string iso=lowerStr(c.cc.empty()?isoForCountry(c.country):c.cc);
    std::string key=c.id+"|"+std::to_string(c.variant)+"|"+(pcPhoto.tex?"1":"0")+"|"+(flagTex.count(iso)?"1":"0");
    if(key!=pcFaceKey){ pcBuildFace(c); pcFaceKey=key; }
    float fw=(float)pcFaceW,fh=(float)pcFaceH;
    if(pcFace&&fw>0){
        float sc=.84f+.16f*e, ang=-8.f+8.f*e+sinf(t*.55f)*1.2f;
        float dw=fw*sc,dh=fh*sc;
        float cx=W/2.f,cy=H/2.f+(1.f-e)*46.f+sinf(t*.8f)*3.f;
        pcShadow(b,cx+10,cy+dh*.46f,dw*.40f,dh*.055f,alpha(C(0,0,0),(int)(160*e)),26); b.flush(ren);
        SDL_Rect src{0,0,pcFaceW,pcFaceH};
        SDL_Rect dst{(int)(cx-dw/2),(int)(cy-dh/2),(int)dw,(int)dh};
        pcRect=dst;
        SDL_SetTextureAlphaMod(pcFace,(Uint8)(255*e));
        SDL_RenderCopyEx(ren,pcFace,&src,&dst,ang,nullptr,SDL_FLIP_NONE);
    }
    std::string head=trunc(F17,"CITY POSTCARD · "+upperStr(c.city),W-200,true);
    pcSpaced(head,W/2.f-tw(F17,head,true)/2.f-(float)head.size(),34,F17,2.f,alpha(Pal::dim,(int)(220*e)));
    auto chip2=[&](float x,float y,float w,float h,int id,const char* label){
        b.rrect2(x,y,w,h,11,9,alpha(Pal::chip,(int)(235*e)));
        b.line(x+10,y+1,x+w-10,y+1,1,alpha(Pal::wire,(int)(18*e))); b.flush(ren);
        txt(F17,label,(int)(x+w/2),(int)(y+h/2-10),alpha(Pal::text,(int)(240*e)),true,1);
        hit(id,(int)x,(int)y,(int)w,(int)h);
    };
    chip2(W/2.f-84,H-72,168,44,H_PC_CLOSE,"B · CLOSE");
    if(c.photoState==2)chip2(W/2.f-84,H-124,168,40,H_PC_RETRY,"Y · FIND PHOTO");
}
    void drawTabs(float px,float py){
        const char* n[4]={"FAVORITES","HISTORY","PRESETS","SOUND"};
        float tw2=(PW-32)/4.f;
        for(int i=0;i<4;i++){
            float x=px+16+i*tw2; bool on=(libTab==i);
            if(on)b.rrect2(x+2,py+52,tw2-4,26,8,6,alpha(Pal::wire,14));
            b.flush(ren);
            txt(F15,n[i],(int)(x+tw2/2),(int)py+58,on?Pal::text:Pal::faint,on,1);
            if(on){ b.line(x+6,py+78,x+tw2-6,py+78,2,Pal::accent); b.flush(ren); }
            hit(H_TAB,(int)x,(int)py+50,(int)tw2,30,i);
        }
    }
void drawPanel(){
    if(panel<.01f)return;
    float px=panelX(),py=84,ph=panelH();
    plate(px,py,PW,ph,16,alpha(Pal::chip,250)); b.flush(ren);
    float lx=px+16,lw=PW-32,sepY=py+86;
    if(screen==SCR_CITY){
        Icon::back(b,px+30,py+34,11,Pal::dim); b.flush(ren);
        hit(H_BACK,(int)px+12,(int)py+14,40,40);
        txt(F30,trunc(F30,curCity.title,PW-140,true),(int)px+56,(int)py+16,Pal::text,true);
        txt(F17,curCity.country,(int)px+56,(int)py+52,Pal::dim);
        {
            float bx=px+PW-58,by=py+16;
            b.rrect2(bx,by,42,42,10,8,alpha(Pal::wire,14)); b.flush(ren);
            PcIcon::card(b,bx+21,by+21,11,Pal::warm); b.flush(ren);
            hit(H_CARD,(int)bx,(int)by,42,42,-2);
        }
        sepY=py+82;
    } else if(screen==SCR_SEARCH){
        txt(F30,"Search",(int)px+20,(int)py+14,Pal::text,true);
        b.rrect(lx,py+56,lw,52,10,Pal::cardHi); b.flush(ren);
        hit(H_FIELD,(int)lx,(int)py+56,(int)lw,52);
        Icon::search(b,lx+24,py+82,9,Pal::dim); b.flush(ren);
        std::string sh=query.empty()?"Stations, cities…":query;
        txt(F20,trunc(F20,sh,(int)lw-70),(int)lx+46,(int)py+68,query.empty()?Pal::faint:Pal::text);
        sepY=py+118;
    } else {
        Icon::back(b,px+30,py+30,11,Pal::dim); b.flush(ren);
        hit(H_BACK,(int)px+12,(int)py+10,40,40);
        txt(F30,"Library",(int)px+56,(int)py+12,Pal::text,true);
        drawTabs(px,py); sepY=py+86;
    }
    b.line(px+20,sepY,px+PW-20,sepY,1,alpha(Pal::wire,16)); b.flush(ren);
    float listTop=sepY+12,listH=py+ph-16-listTop;
    SDL_Rect clip{(int)lx,(int)listTop,(int)lw,(int)listH};
    SDL_RenderSetClipRect(ren,&clip);
    clipOn=true; clipR=clip;
    panelListRect=clip; panelListOn=true;
    listScrollMax=std::max(0.f,rowsH-listH);
    bool loading=(screen==SCR_CITY&&cityLoading)||(screen==SCR_SEARCH&&searchLoading);
    if(loading){
        Icon::spinner(b,px+PW/2,listTop+60,16,t*4,Pal::accent); b.flush(ren);
        txt(F17,"Tuning in…",(int)px+PW/2,(int)listTop+92,Pal::dim,false,1);
    } else if(rows.empty()){
        std::string msg=screen==SCR_CITY?"No stations here":
            screen==SCR_SEARCH?(query.size()<2?"Type at least 2 characters":"Nothing found"):
            (libTab==LT_FAV?"Empty. Save a station with the favorite button.":"No tracks logged yet");
        txt(F17,msg,(int)px+PW/2,(int)listTop+60,Pal::dim,false,1);
        if(screen==SCR_CITY&&!cityRes.err.empty())
            txt(F15,trunc(F15,cityRes.err,(int)lw-20),(int)px+PW/2,(int)listTop+86,Pal::faint,false,1);
    } else {
        for(size_t i=0;i<rows.size();i++){
            Row& r=rows[i];
            float y=listTop+r.y-scroll;
            if(y+r.h<listTop||y>listTop+listH)continue;
            if(r.kind==0){
                txt(F15,trunc(F15,r.title,(int)lw-8),(int)lx+4,(int)y+10,Pal::faint);
                b.line(lx+4,y+30,lx+lw-4,y+30,1,alpha(Pal::wire,10)); b.flush(ren);
                continue;
            }
            bool selr=(r.sel==selIdx),playing=now.valid&&r.kind==1&&now.chId==r.ch.id;
            bool fav=r.kind==1&&isFav(r.ch.id);
            float rh=r.h-10;
            b.rrect2(lx,y,lw,rh,10,7,Pal::card); b.flush(ren);
            txt(F20,trunc(F20,r.title,(int)lw-(r.kind==1?130:40),selr),(int)lx+20,(int)y+10,
                playing?Pal::liveSoft:Pal::text,selr);
            std::string sub=playing?"now playing":r.sub;
            if(!sub.empty())txt(F15,trunc(F15,sub,(int)lw-(r.kind==1?130:40)),(int)lx+20,(int)y+36,Pal::dim);
            if(r.kind==1){
                if(fav)Icon::heart(b,lx+lw-62,y+rh/2,9,Pal::warm,true);
                b.circle(lx+lw-30,y+rh/2,14,alpha(Pal::wire,selr?26:12),12); b.flush(ren);
                Icon::play(b,lx+lw-28,y+rh/2,8,selr?Pal::accentSoft:Pal::faint); b.flush(ren);
            }
            else if(r.kind==2){ Icon::globe(b,lx+lw-30,y+rh/2,9,selr?Pal::accentSoft:Pal::faint); b.flush(ren); }
            else if(r.kind==3){
                b.rrect2(lx+lw-84,y+rh/2-13,68,26,8,6,alpha(Pal::wire,selr?22:10)); b.flush(ren);
                txt(F15,r.sub,(int)(lx+lw-50),(int)(y+rh/2-8),selr?Pal::text:Pal::dim,true,1);
            }
            else if(r.kind==4)
                txt(F24,std::to_string(r.slot+1),(int)(lx+lw-34),(int)(y+rh/2-14),
                    state.presets[r.slot].set?Pal::warm:Pal::faint,true,1);
            else if(r.kind==6){
                PcIcon::card(b,lx+lw-30,y+rh/2,10,selr?Pal::warm:Pal::faint); b.flush(ren);
            }
            hit(H_ROW,(int)lx,(int)y,(int)lw,(int)rh,r.sel);
        }
        if(!selRows.empty()&&selIdx<(int)selRows.size()){
            float fy=listTop+focusY-scroll;
            float rh=rows[selRows[selIdx]].h-10;
            b.rrect2(lx,fy,lw,rh,10,7,alpha(Pal::wire,12));
            b.line(lx+3,fy+12,lx+3,fy+rh-12,2.4f,Pal::accent);
            b.flush(ren);
        }
    }
    SDL_RenderSetClipRect(ren,nullptr); clipOn=false;
}
    void drawFlag(float x,float y,float w,float h,const std::string& cc,const std::string& country){
        std::string iso=lowerStr(cc.empty()?isoForCountry(country):cc);
        if(!iso.empty())requestFlag(iso);
		if(!iso.empty())flagUse[iso]=frameNo;
        auto it=flagTex.find(iso);
        if(it!=flagTex.end()&&it->second){
            int tw2=0,th2=0;
            SDL_QueryTexture(it->second,nullptr,nullptr,&tw2,&th2);
            if(tw2>0&&th2>0){
                float sc=std::min(w/tw2,h/th2),dw=tw2*sc,dh=th2*sc;
                float dx=x+(w-dw)/2,dy=y+(h-dh)/2;
                SDL_Rect d{(int)dx,(int)dy,(int)dw,(int)dh};
                SDL_RenderCopy(ren,it->second,nullptr,&d);
                b.line(dx,dy,dx+dw,dy,1,alpha(Pal::wire,45));
                b.line(dx,dy+dh,dx+dw,dy+dh,1,alpha(Pal::wire,45));
                b.line(dx,dy,dx,dy+dh,1,alpha(Pal::wire,28));
                b.line(dx+dw,dy,dx+dw,dy+dh,1,alpha(Pal::wire,28));
                b.flush(ren); return;
            }
        }
        b.rrect(x,y,w,h,6,alpha(Pal::wire,10)); b.flush(ren);
        txt(F17,iso.empty()?"??":upperStr(iso),(int)(x+w/2),(int)(y+h/2-10),Pal::dim,true,1);
    }
    void drawTuning(float x,float y,float w,float h){
        b.line(x,y+h*.6f,x+w,y+h*.6f,1.4f,alpha(Pal::wire,40));
        for(int i=0;i<=10;i++){
            float xx=x+w*i/10.f,hh=(i%5==0)?9.f:5.f;
            b.line(xx,y+h*.6f-hh*.5f,xx,y+h*.6f+hh*.5f,1.2f,alpha(Pal::wire,i%5==0?70:35));
        }
        float st=clampf(tuneAnim/1.2f,0,1);
        float nx=x+w*(.5f+sinf(tuneAnim*7.f)*(1.f-st)*.42f);
        b.line(nx,y+2,nx,y+h-2,2.2f,Pal::warm);
        b.circle(nx,y+2,2.6f,Pal::warmSoft,8); b.flush(ren);
        txt(F15,"TUNING",(int)(x+w/2),(int)(y+h-4),Pal::faint,false,1);
    }
    void drawNowCard(){
        if(!now.valid)return;
        const float h=284.f, w=470.f;
        float x,y;
        if(panel>.5f){ x=panelX()+PW-w; y=84+panelH()+12; }
        else { x=W-24-w; y=190; }
        nowCardRect={(int)x,(int)y,(int)w,(int)h}; nowCardOn=true;
        int st=player.state;
        bool live=st==Player::PLAYING;
        SDL_Color tint=skyTint(cachedHour);
        plate(x,y,w,h,16,alpha(mixc(C(0,0,0),tint,.16f),252));
        b.gradRect(x+2,y+2,w-4,h*.5f,alpha(tint,26),alpha(tint,0));
        b.rrect(x+18,y+1,w-36,2.5f,1.2f,alpha(Pal::live,live?235:80)); b.flush(ren);
        float pulse=.6f+.4f*sinf(t*3.f);
        b.circle(x+24,y+27,3.5f,alpha(live?Pal::live:Pal::faint,live?(int)(160+90*pulse):150),8); b.flush(ren);
        txt(F17,"NOW PLAYING",(int)x+38,(int)y+17,Pal::dim);
        std::string badge=live?"ON AIR":st==Player::BUFFERING?"TUNING":st==Player::PAUSED?"PAUSED":"OFFLINE";
        SDL_Color bc=live?Pal::liveSoft:st==Player::FAILED?Pal::danger:Pal::dim;
        int bw2=tw(F17,badge,true)+24;
        b.rrect(x+w-18-bw2,y+14,(float)bw2,26,13,alpha(bc,38)); b.flush(ren);
        txt(F17,badge,(int)(x+w-18-bw2/2),(int)y+17,bc,true,1);
        if(nowGta){
            std::string logo=gtaLogoRel(gtaNowMount);
            if(!gtaAssetPath(logo).empty())gtaLogo(logo,SDL_Rect{(int)x+20,(int)y+50,56,56},128,255,255);
            else{
                std::string game=gtaNowMount.substr(0,gtaNowMount.find('/'));SDL_Color ac=hsv(gtaHue(game),.6f,.95f);
                b.rrect(x+20,y+50,56,56,12,alpha(ac,26));b.flush(ren);
                GtaIcon::wheel(b,x+48,y+78,20,alpha(Pal::text,220),ac);b.flush(ren);
            }
        } else drawFlag(x+20,y+50,56,56,now.cc,now.country);
        marquee(F30,now.title,(int)x+92,(int)y+46,(int)w-116,Pal::text,true,30.f);
        std::string sub=now.place.empty()?now.country:now.place+" · "+now.country;
        txt(F17,trunc(F17,sub,(int)w-116),(int)x+92,(int)y+86,Pal::dim);
        hit(H_PLACE,(int)x+92,(int)y+82,(int)w-116,26);
        float colW=(w-40)/2.f,c1=x+20,c2=x+20+colW;
        b.line(x+20,y+120,x+w-20,y+120,1,alpha(Pal::wire,12)); b.flush(ren);
        Icon::clock(b,c1+11,y+144,11,Pal::accentSoft); b.flush(ren);
        txt(F24,localStr,(int)c1+32,(int)y+130,Pal::text,true);
        txt(F15,"LOCAL TIME",(int)c1+32,(int)y+160,Pal::faint);
        if(nowWx.ok){
            Icon::weather(b,c2+12,y+144,12,nowWx.code,Pal::warm); b.flush(ren);
            char tb[16]; snprintf(tb,16,"%d°C",(int)lroundf(nowWx.temp));
            txt(F24,tb,(int)c2+34,(int)y+130,Pal::text,true);
            txt(F15,trunc(F15,wxCond(nowWx.code),(int)colW-44),(int)c2+34,(int)y+160,Pal::faint);
        } else {
            Icon::cloud(b,c2+12,y+144,11,alpha(Pal::faint,150)); b.flush(ren);
            txt(F24,"--",(int)c2+34,(int)y+130,Pal::faint,true);
            txt(F15,"WEATHER",(int)c2+34,(int)y+160,Pal::faint);
        }
        float ex=x+20,ew=w-40,ey=y+184,eh=34;
        if(st==Player::BUFFERING)drawTuning(ex,ey,ew,eh);
        else {
            float bw=ew/NB;
            b.line(ex,ey+eh+2,ex+ew,ey+eh+2,1,alpha(Pal::wire,10));
            for(int i=0;i<NB;i++){
                float v=eq[i];
                float bh=2.5f+v*eh;
                SDL_Color col=alpha(live?(v>.78f?Pal::accentSoft:Pal::accent):Pal::faint,215);
                float bx=ex+i*bw+1.2f,by=ey+eh-bh,wi=bw-2.4f;
                b.quad(bx,by,bx+wi,by,bx+wi,by+bh,bx,by+bh,col);
            }
            b.flush(ren);
        }
        float cy=y+250;
        bool fv=isFav(now.chId);
        b.circle(x+44,cy,22,live?Pal::live:alpha(Pal::wire,18),18);
        b.ring(x+96,cy,18,1.3f,alpha(Pal::wire,26),16);
        Icon::heart(b,x+96,cy,10,fv?Pal::warm:Pal::dim,fv);
        if(live||st==Player::BUFFERING)Icon::pause(b,x+44,cy,10.5f,C(0,0,0));
        else Icon::play(b,x+44,cy,10.5f,st==Player::FAILED?Pal::danger:Pal::text);
        b.flush(ren);
        hit(H_PLAYPAUSE,(int)x+22,(int)cy-22,44,44);
        hit(H_FAVNOW,(int)x+76,(int)cy-20,40,40);

        std::string status;
        if(st==Player::BUFFERING)status="Connecting…";
        else if(st==Player::FAILED)status=player.error();
        else if(st==Player::PAUSED)status="Paused";
        else status=player.trackTitle();
        if(status.empty())status=now.place.empty()?now.country:now.place;
        if(sleepMin>0&&sleepLeft>0){ char sb[24]; snprintf(sb,24,"%dm · ",(int)ceilf(sleepLeft/60.f)); status=sb+status; }
        marquee(F17,status,(int)x+126,(int)cy-10,(int)w-146,
                st==Player::FAILED?Pal::danger:Pal::faint,false,46.f);
}
    void drawTopBar(){
        b.gradRect(0,0,W,72,alpha(C(0,0,0),170),alpha(C(0,0,0),0)); b.flush(ren);
        Icon::globe(b,34,32,15,Pal::text); b.flush(ren);
        txt(F24,"RADIO",58,19,Pal::text,true);
        int lx=58+tw(F24,"RADIO",true)+10;
        b.rrect((float)lx,20,36,24,5,Pal::accent); b.flush(ren);
        txt(F15,"NX",lx+18,24,C(255,255,255),true,1);
        txt(F24,clockStr,W-28,19,Pal::text,true,2);
    }
    void drawToasts(float y0=84.f){
        float y=y0;
        for(auto& to:toasts){
            float a=clampf(to.age<.25f?to.age/.25f:(to.age>4.f?(4.5f-to.age)/.5f:1.f),0,1);
            std::string s=trunc(F17,to.text,W-120);
            int w=tw(F17,s)+44;
            float x=W/2.f-w/2.f;
            b.rrect2(x,y,(float)w,38,11,8,alpha(Pal::chip,(int)(248*a)));
            b.line(x+10,y+1,x+w-10,y+1,1,alpha(Pal::wire,(int)(16*a))); b.flush(ren);
            txt(F17,s,(int)W/2,(int)y+9,alpha(to.col,(int)(255*a)),false,1);
            y+=46;
        }
    }
    void drawOSD(){
        if(osdVol<=0)return;
        float a=clampf(osdVol/.4f,0,1);
        char buf[16]; snprintf(buf,16,"VOL %d",(int)lroundf(player.volume.load()*100));
        float w=170,h=44,x=W/2.f-w/2,y=H-96;
        b.rrect2(x,y,w,h,12,9,alpha(Pal::chip,(int)(245*a))); b.flush(ren);
        txt(F20,buf,(int)(x+18),(int)y+11,alpha(Pal::text,(int)(255*a)),true);
        float bx=x+96,bw=56;
        b.quad(bx,y+21,bx+bw,y+21,bx+bw,y+25,bx,y+25,alpha(Pal::wire,(int)(40*a)));
        float vw=bw*player.volume.load();
        b.quad(bx,y+21,bx+vw,y+21,bx+vw,y+25,bx,y+25,alpha(Pal::accent,(int)(230*a)));
        b.flush(ren);
    }
    void drawLoading(){
        if(placesReady)return;
        float w=460,h=130,x=W/2.f-w/2,y=H/2.f-h/2;
        plate(x,y,w,h,16,alpha(Pal::chip,250)); b.flush(ren);
        if(placesError){
            txt(F20,"Couldn't load the map",(int)W/2,(int)y+34,Pal::danger,true,1);
            txt(F17,"select or click to retry",(int)W/2,(int)y+72,Pal::dim,false,1);
            hit(H_RETRY,(int)x,(int)y,(int)w,(int)h);
        } else {
            Icon::spinner(b,x+50,y+h/2,18,t*4,Pal::accent); b.flush(ren);
            txt(F20,"Listening to the world…",(int)x+90,(int)y+38,Pal::text,true);
            txt(F15,"first launch takes a few seconds",(int)x+90,(int)y+72,Pal::dim);
        }
    }
    void drawNightOverlay(){
        if(nightMix<.01f)return;
        SDL_SetRenderDrawBlendMode(ren,SDL_BLENDMODE_BLEND);
        SDL_SetRenderDrawColor(ren,0,0,0,(Uint8)(150*nightMix));
        SDL_Rect full{0,0,W,H}; SDL_RenderFillRect(ren,&full);
        if(!now.valid)return;
        float w=560,h=86,x=W/2.f-w/2,y=H-120;
        b.rrect2(x,y,w,h,14,10,alpha(Pal::chip,(int)(230*nightMix))); b.flush(ren);
        b.circle(x+26,y+26,3.5f,alpha(Pal::live,(int)(200*nightMix)),8); b.flush(ren);
        txt(F20,trunc(F20,now.title,(int)w-160,true),(int)x+42,(int)y+16,alpha(Pal::text,(int)(255*nightMix)),true);
        std::string sub=now.place.empty()?now.country:now.place+" · "+now.country;
        txt(F15,trunc(F15,sub,(int)w-160),(int)x+42,(int)y+46,alpha(Pal::dim,(int)(255*nightMix)));
        float ex=x+w-140,ew=118,ey=y+22,eh=42,bw=ew/16;
        for(int i=0;i<16;i++){
            float bh=2.5f+eq[i*2]*eh,bx=ex+i*bw+1.f;
            b.quad(bx,ey+eh-bh,bx+bw-2.f,ey+eh-bh,bx+bw-2.f,ey+eh,bx,ey+eh,alpha(Pal::accent,(int)(200*nightMix)));
        }
        b.flush(ren);
        txt(F15,"press any button",(int)(x+w/2),(int)(y+h+8),alpha(Pal::faint,(int)(200*nightMix)),false,1);
    }
    void drawClean(){
        float A=clampf(cleanMix,0.f,1.f);
        if(A<.02f)return;
        auto AL=[&](int a){ return (int)(a*A); };
        int st=player.state;
        bool live=st==Player::PLAYING;
        const int LX=56, LW=286, RX=W-56;
        if(now.valid){
            if(A>.35f){
                if(nowGta){SDL_Color ac=hsv(38.f,.6f,.95f);GtaIcon::wheel(b,(float)LX+30,312.f,20,alpha(Pal::text,AL(210)),alpha(ac,AL(230)));b.flush(ren);}
                else drawFlag((float)LX,292.f,60.f,40.f,now.cc,now.country);
            }
            std::string nm=trunc(F24,now.title,LW,true);
            txt(F24,nm,LX,344,alpha(Pal::text,AL(255)),true);
            int uw=std::min(tw(F24,nm,true),LW);
            b.line((float)LX,382.f,(float)(LX+uw),382.f,2.f,
                   alpha(live?Pal::live:Pal::faint,AL(live?210:80)));
            b.flush(ren);
            if(!now.place.empty())txt(F17,trunc(F17,now.place,LW),LX,394,alpha(Pal::dim,AL(235)));
            if(!now.country.empty())txt(F15,trunc(F15,now.country,LW),LX,418,alpha(Pal::faint,AL(215)));
            if(!live){
                std::string s=st==Player::BUFFERING?"TUNING…":st==Player::PAUSED?"PAUSED":"OFFLINE";
                txt(F15,s,LX,444,alpha(st==Player::FAILED?Pal::danger:Pal::warm,AL(220)),true);
            } else {
                std::string tr=player.trackTitle();
                if(!tr.empty())txt(F15,trunc(F15,tr,LW),LX,444,alpha(Pal::faint,AL(190)));
            }
        } else {
            txt(F24,"No station",LX,344,alpha(Pal::faint,AL(220)),true);
            txt(F15,"press Y for a random wave",LX,382,alpha(Pal::faint,AL(170)));
        }
        txt(F15,"MY TIME",RX,286,alpha(Pal::faint,AL(200)),false,2);
        txt(F30,clockStr,RX,306,alpha(Pal::text,AL(255)),true,2);
        if(now.valid){
            txt(F15,"STATION LOCAL",RX,362,alpha(Pal::faint,AL(200)),false,2);
            txt(F30,localStr,RX,382,alpha(Pal::accentSoft,AL(255)),true,2);
            if(nowWx.ok){
                char tb[16]; snprintf(tb,16,"%d°C",(int)lroundf(nowWx.temp));
                Icon::weather(b,(float)RX-118.f,452.f,13.f,nowWx.code,alpha(Pal::warm,AL(235))); b.flush(ren);
                txt(F30,tb,RX,436,alpha(Pal::text,AL(255)),true,2);
                txt(F15,trunc(F15,wxCond(nowWx.code),190),RX,474,alpha(Pal::faint,AL(205)),false,2);
            } else {
                Icon::cloud(b,(float)RX-118.f,452.f,12.f,alpha(Pal::faint,AL(150))); b.flush(ren);
                txt(F30,"--",RX,436,alpha(Pal::faint,AL(190)),true,2);
                txt(F15,"WEATHER",RX,474,alpha(Pal::faint,AL(170)),false,2);
            }
        }
        if(osdVol>0){
            float va=clampf(osdVol/.4f,0.f,1.f)*A;
            char vb[16]; snprintf(vb,16,"VOL %d",(int)lroundf(player.volume.load()*100));
            txt(F17,vb,W/2,640,alpha(Pal::text,(int)(235*va)),true,1);
        }

    }
    void draw(){
        frameNo++;uiHits.clear();clipOn=false;
        SDL_RenderSetClipRect(ren,nullptr);
        panelListOn=false;nowCardOn=false;
        if(bgTex)SDL_RenderCopy(ren,bgTex,nullptr,nullptr);
        else { SDL_SetRenderDrawColor(ren,0,0,0,255); SDL_RenderClear(ren); }
        float hA=homeMix;
        if(hA>.01f){ b.gradRect(0,0,W,H*.75f,alpha(C(18,24,58),(int)(70*hA)),alpha(C(0,0,0),0)); b.flush(ren); }
        float nb=homeMode?1.f:(now.valid?((cachedHour<6.f||cachedHour>19.f)?1.f:0.f):0.f);
        float warm=(!homeMode&&now.valid)?(1.f-nb)*.6f:0.f;
        sky.draw(b,t,sinf(globe.yaw)*.5f,(cleanMode&&!homeMode)?1.f:nb); b.flush(ren);
if(pcMix<.999f&&passportMix<.999f)globe.draw(ren,b,t,W,H,warm);
        if(gtaMix<.06f&&nowGta&&!homeMode&&pcMix<.5f&&passportMix<.5f)gtaAura(.85f);
        if(gtaMix<.06f)stMapGlow();
        if(hA>=.02f){ drawAurora(hA); drawHome(hA); }
        if(hA<.02f&&gtaMix<.01f&&!gtaMode&&!homeMode){
            if(cleanMode){
                drawClean();
                drawToasts();
                drawLoading();
            } else {
                if(nightMix<.6f){
                    if(screen==SCR_GLOBE&&globe.nearest>=0&&(size_t)globe.nearest<places.size()
                       &&globe.nearestZ>.9f&&placesReady){
                        const Place& pl=places[globe.nearest];
                        char cnt[32]; snprintf(cnt,32,"%d stations",pl.size);
                        int w=std::max(tw(F20,pl.title,true),std::max(tw(F15,pl.country),tw(F15,cnt)))+40;
                        float x=globe.center.x-w/2.f,y=std::min(globe.center.y+globe.R+16,(float)H-160);
                        plate(x,y,(float)w,76,12,alpha(Pal::chip,245)); b.flush(ren);
                        txt(F20,pl.title,(int)globe.center.x,(int)y+8,Pal::text,true,1);
                        txt(F15,pl.country,(int)globe.center.x,(int)y+32,Pal::dim,false,1);
                        txt(F15,cnt,(int)globe.center.x,(int)y+52,Pal::liveSoft,false,1);
                    }
                    drawTopBar(); drawPanel(); drawNowCard(); drawToasts();
                    drawLoading();
                }
                drawOSD(); drawNightOverlay();
            }
        }
        if(hA>=.02f){
            if(hA>=.55f)drawToasts(112.f);
            if(!placesReady)
                txt(F15,placesError?"map unavailable":"loading map…",24,76,alpha(Pal::faint,(int)(190*hA)));
            if(now.valid){
                bool live=player.state==Player::PLAYING;
                b.circle(30,H-30,3.5f,alpha(live?Pal::live:Pal::faint,(int)(210*hA)),8); b.flush(ren);
                txt(F17,trunc(F17,now.title,360),44,H-40,alpha(Pal::dim,(int)(230*hA)));
            }
        }
        if(gtaMix>.01f){uiHits.clear();drawGta();}
        if(passportMix>.5f)uiHits.clear();
        drawPassport();
        if(pcMix>.5f)uiHits.clear();
        drawPostcard();
        SDL_RenderPresent(ren);
    }
  void onButton(int btn){
    bool wasScreenOff=screenOff;
    wake();
    if(btn==SDL_CONTROLLER_BUTTON_RIGHTSTICK){
        if(!wasScreenOff)setScreenOff(true);
        return;
    }
    if(pcOpen){
        if(btn==BTN_BACK||btn==BTN_SELECT||btn==SDL_CONTROLLER_BUTTON_BACK||btn==SDL_CONTROLLER_BUTTON_START)pcClose();
        else if(btn==BTN_RANDOM)pcRetry();
        return;
    }
    if(passportOpen){
        if(btn==BTN_BACK||btn==SDL_CONTROLLER_BUTTON_BACK)closePassport();
        else if(btn==SDL_CONTROLLER_BUTTON_DPAD_LEFT)ppTurnPage(-1);
        else if(btn==SDL_CONTROLLER_BUTTON_DPAD_RIGHT)ppTurnPage(1);
        else if(btn==BTN_RANDOM){stMapUntil=t+8.f;closePassport();}
        return;
    }
    if(gtaMode){
        if(btn==BTN_BACK){if(gtaView==1)gtaBackToShelf();else{exitGta();enterHome();}return;}
        if(gtaView==0){
            switch(btn){
                case SDL_CONTROLLER_BUTTON_DPAD_LEFT:gtaMoveCard(-1,0);break;
                case SDL_CONTROLLER_BUTTON_DPAD_RIGHT:gtaMoveCard(1,0);break;
                case SDL_CONTROLLER_BUTTON_DPAD_UP:gtaMoveCard(0,-1);break;
                case SDL_CONTROLLER_BUTTON_DPAD_DOWN:gtaMoveCard(0,1);break;
                case SDL_CONTROLLER_BUTTON_B:gtaEnterGame(gtaPick);break;
                case SDL_CONTROLLER_BUTTON_LEFTSTICK:player.togglePause();break;
            }
            return;
        }
        if(btn==BTN_SELECT){
            if(!gtaRows.empty()&&gtaSel>=0&&gtaSel<(int)gtaRows.size()){
                const GtaStation& station=gta[gtaRows[gtaSel]];
                if(nowGta&&gtaNowMount==station.mount)player.togglePause();else gtaPlay(gtaSel);
            }
            return;
        }
        if(btn==BTN_FAV){gtaFav();return;}
        if(btn==BTN_RANDOM){gtaRandom();return;}
        switch(btn){
            case SDL_CONTROLLER_BUTTON_DPAD_LEFT:
            case SDL_CONTROLLER_BUTTON_DPAD_UP:gtaMove(-1);break;
            case SDL_CONTROLLER_BUTTON_DPAD_RIGHT:
            case SDL_CONTROLLER_BUTTON_DPAD_DOWN:gtaMove(1);break;
            case SDL_CONTROLLER_BUTTON_LEFTSHOULDER:gtaNextGame(-1);break;
            case SDL_CONTROLLER_BUTTON_RIGHTSHOULDER:gtaNextGame(1);break;
            case SDL_CONTROLLER_BUTTON_LEFTSTICK:player.togglePause();break;
        }
        return;
    }
    if(btn==SDL_CONTROLLER_BUTTON_BACK||btn==SDL_CONTROLLER_BUTTON_START){
        if(btn==SDL_CONTROLLER_BUTTON_BACK)minusDown=true; else plusDown=true;
        if(minusDown&&plusDown&&!comboFired){ comboFired=true; if(homeMode)enterRadio(); toggleClean(); }
        return;
    }
    if(nightMix>.5f)return;
    if(homeMode){
        if(btn==SDL_CONTROLLER_BUTTON_DPAD_LEFT){ home.move(-1); audio.sfx.trigger(Sfx::TICK); }
        else if(btn==SDL_CONTROLLER_BUTTON_DPAD_RIGHT){ home.move(1); audio.sfx.trigger(Sfx::TICK); }
        else if(btn==BTN_SELECT)homeActivate();
        else if(btn==BTN_RANDOM){ enterRadio(); randomWave(); }
        else if(btn==SDL_CONTROLLER_BUTTON_LEFTSTICK)player.togglePause();
        return;
    }
    if(btn==BTN_SELECT){ pressConfirm(); return; }
    if(btn==BTN_BACK){ pressBack(); return; }
    if(btn==BTN_FAV){ favSelected(); return; }
    if(btn==BTN_RANDOM){ randomWave(); return; }
    switch(btn){
        case SDL_CONTROLLER_BUTTON_LEFTSTICK: player.togglePause(); break;
        case SDL_CONTROLLER_BUTTON_DPAD_UP:
            if(screen==SCR_GLOBE||cleanMode){ globe.flying=false; globe.idle=0; globe.pitch=clampf(globe.pitch+.12f*globe.inputK(),-1.35f,1.35f); }
            else moveSel(-1);
            break;
        case SDL_CONTROLLER_BUTTON_DPAD_DOWN:
            if(screen==SCR_GLOBE||cleanMode){ globe.flying=false; globe.idle=0; globe.pitch=clampf(globe.pitch-.12f*globe.inputK(),-1.35f,1.35f); }
            else moveSel(1);
            break;
        case SDL_CONTROLLER_BUTTON_DPAD_LEFT:
            if(cleanMode){ globe.flying=false; globe.idle=0; globe.yaw+=.12f*globe.inputK(); }
            else if(screen==SCR_LIB)switchTab(-1);
            else if(screen==SCR_GLOBE){ globe.flying=false; globe.idle=0; globe.yaw+=.12f*globe.inputK(); }
            else moveSel(-1);
            break;
        case SDL_CONTROLLER_BUTTON_DPAD_RIGHT:
            if(cleanMode){ globe.flying=false; globe.idle=0; globe.yaw-=.12f*globe.inputK(); }
            else if(screen==SCR_LIB)switchTab(1);
            else if(screen==SCR_GLOBE){ globe.flying=false; globe.idle=0; globe.yaw-=.12f*globe.inputK(); }
            else moveSel(1);
            break;
    }
}
    void onButtonUp(int btn){
        if(btn==SDL_CONTROLLER_BUTTON_BACK){
            minusDown=false;
            if(!comboFired&&nightMix<=.5f){
                if(homeMode){ enterRadio(); openLib(LT_FAV); }
                else {
                    if(cleanMode)setClean(false);
                    if(screen==SCR_LIB)switchTab(1); else openLib(LT_FAV);
                }
            }
        } else if(btn==SDL_CONTROLLER_BUTTON_START){
            plusDown=false;
            if(!comboFired&&nightMix<=.5f){ if(homeMode)enterRadio(); openSearch(); }
        }
        if(!minusDown&&!plusDown)comboFired=false;
    }
        void events(){
        SDL_Event e;
        while(SDL_PollEvent(&e)){
            switch(e.type){
                case SDL_QUIT: running=false; break;
                case SDL_CONTROLLERDEVICEADDED:
                    if(!pad&&SDL_IsGameController(e.cdevice.which))pad=SDL_GameControllerOpen(e.cdevice.which);
                    break;
                case SDL_CONTROLLERDEVICEREMOVED:
                    if(pad&&SDL_JoystickInstanceID(SDL_GameControllerGetJoystick(pad))==e.cdevice.which){
                        SDL_GameControllerClose(pad); pad=nullptr; }
                    minusDown=plusDown=comboFired=false; bl=br=false;
                    break;
                case SDL_CONTROLLERBUTTONDOWN:
                    if(e.cbutton.button==SDL_CONTROLLER_BUTTON_LEFTSHOULDER&&!gtaMode){bl=true;wake();}
                    else if(e.cbutton.button==SDL_CONTROLLER_BUTTON_RIGHTSHOULDER&&!gtaMode){br=true;wake();}
                    else onButton(e.cbutton.button);
                    break;
                case SDL_CONTROLLERBUTTONUP:
                    if(e.cbutton.button==SDL_CONTROLLER_BUTTON_LEFTSHOULDER)bl=false;
                    else if(e.cbutton.button==SDL_CONTROLLER_BUTTON_RIGHTSHOULDER)br=false;
                    else onButtonUp(e.cbutton.button);
                    break;
                case SDL_FINGERDOWN:{
                    wake();
                    Vec2 p{e.tfinger.x*W,e.tfinger.y*H};
                    touch[e.tfinger.fingerId]=p; fingers=(int)touch.size();
                    if(fingers==1)touchBegin(p.x,p.y,e.tfinger.fingerId);
                    else { tActive=false; pinchDist=0; scrollVel=0; }
                    break;}
                case SDL_FINGERUP:{
                    Vec2 p{e.tfinger.x*W,e.tfinger.y*H};
                    bool wasPrimary=(e.tfinger.fingerId==tPrimary);
                    touch.erase(e.tfinger.fingerId); fingers=(int)touch.size();
                    if(fingers<2)pinchDist=0;
                    if(wasPrimary&&tActive)touchEnd(p.x,p.y);
                    tActive=false;
                    if(fingers==1){
                        auto it=touch.begin();
                        tPrimary=it->first; tStart=tPrev=tCur=it->second;
                        tTravel=999.f; tActive=true; tDownT=SDL_GetTicks();
                        tZone=touchZone(it->second.x,it->second.y);
                    }
                    break;}
                case SDL_FINGERMOTION:{
                    Vec2 p{e.tfinger.x*W,e.tfinger.y*H};
                    touch[e.tfinger.fingerId]=p; fingers=(int)touch.size(); idleT=0;
                    if(fingers==2&&!passportOpen&&!pcOpen&&!homeMode&&!gtaMode){
                        auto it=touch.begin(); Vec2 a=it->second,b2=(++it)->second;
                        float d=sqrtf((a.x-b2.x)*(a.x-b2.x)+(a.y-b2.y)*(a.y-b2.y));
                        if(pinchDist>0)globe.zoom((d-pinchDist)*.9f);
                        pinchDist=d; tActive=false;
                    } else if(fingers==1&&e.tfinger.fingerId==tPrimary){
                        touchMove(p.x,p.y);
                    }
                    break;}
            }
        }
    }
void run(){
    last=SDL_GetTicks();
    while(running){
        Uint32 fs=SDL_GetTicks();
        float dt=clampf((fs-last)/1000.f,.0005f,focused?.05f:.5f); last=fs;
        events();
        AppletFocusState fst=appletGetFocusState();
        focused=(fst==AppletFocusState_InFocus);
        bool idle=(!focused||screenOff);
        if(idle)updateIdle(dt); else update(dt);
        if(focused&&!screenOff)draw();
        Uint32 el=SDL_GetTicks()-fs;
        Uint32 budget=idle?100u:16u;
        if(el<budget)SDL_Delay(budget-el);
    }
}
    void shutdown(){
    if(didInit){
        stLeave();
        stSave(stamps);
        ppSave(pp);
        flushState(); flushDead();
    }
    stAnimIdx=-1; stAnimTex=nullptr; stAnimTexIdx=-1;
    stFreeTex();
    for(auto& kv:stOvr)if(kv.second)SDL_DestroyTexture(kv.second);
    stOvr.clear(); stOvrTried.clear();
    pcFreePhoto();
    if(pcFace){SDL_DestroyTexture(pcFace);pcFace=nullptr;}
    ppFreePages();
    if(miiTex){SDL_DestroyTexture(miiTex);miiTex=nullptr;}
    if(mediaOn){mediaOn=false;appletSetMediaPlaybackState(false);}
    worker.stop();
    pcWorker.stop();
    gtaWorker.stop();
    gtaImgWorker.stop();
    gtaFreeTx2();
    gtaWorld.destroy();
    flagWorker[0].stop(); flagWorker[1].stop();
    player.stop();
    if(screenOff&&lblInit)lblSetCurrentBrightnessSetting(savedBrightness);
    if(lblInit){lblExit();lblInit=false;}
    audio.close();
    clearLabs();
    globe.destroyCache();
    fonts.close();
    if(bgTex){SDL_DestroyTexture(bgTex);bgTex=nullptr;}
}

};

static void logLine(const char* s){
    static bool ready=false;
    if(!ready){ ready=true; makeDirs("sdmc:/switch/RadioNX/data"); }
    FILE* f=fopen("sdmc:/switch/RadioNX/data/log.txt","a");
    if(!f)f=fopen("sdmc:/RadioNX.log","a");
    if(!f)return;
    fprintf(f,"%s\n",s); fclose(f);
}
int main(int argc,char** argv){
    initPaths(argc>0?argv[0]:nullptr);
    logLine("=== RadioNX start ===");
    socketInitializeDefault();
    sslInitialize(0);
    Result rcRomfs=romfsInit();
    if(R_FAILED(rcRomfs))logLine("romfs: none");
    appletInitialize();
    gAccountOk=R_SUCCEEDED(accountInitialize(AccountServiceType_Application));
    logLine("main: services ok");
    std::unique_ptr<App> app;
    bool ttf=false,curl=false;
    do {
        if(SDL_Init(SDL_INIT_VIDEO|SDL_INIT_AUDIO|SDL_INIT_GAMECONTROLLER)!=0){ SDL_Log("SDL: %s",SDL_GetError()); break; }
        if(TTF_Init()!=0){ logLine("ttf FAIL"); break; }
        ttf=true;
        curl_global_init(CURL_GLOBAL_ALL); curl=true;
        avformat_network_init();
        SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY,"0");
        SDL_SetHint(SDL_HINT_RENDER_VSYNC,"1");
        SDL_GameControllerEventState(SDL_ENABLE);
        Uint32 wf=SDL_WINDOW_SHOWN;
        app.reset(new App());
        app->win=SDL_CreateWindow("RADIO NX",SDL_WINDOWPOS_CENTERED,SDL_WINDOWPOS_CENTERED,W,H,wf);
        if(!app->win){ logLine("window FAIL"); logLine(SDL_GetError()); break; }
        app->ren=SDL_CreateRenderer(app->win,-1,SDL_RENDERER_ACCELERATED|SDL_RENDERER_PRESENTVSYNC);
        if(!app->ren)app->ren=SDL_CreateRenderer(app->win,-1,SDL_RENDERER_SOFTWARE|SDL_RENDERER_PRESENTVSYNC);
        if(!app->ren)app->ren=SDL_CreateRenderer(app->win,-1,SDL_RENDERER_SOFTWARE);
        if(!app->ren){ logLine("renderer FAIL"); logLine(SDL_GetError()); break; }
        SDL_RenderSetLogicalSize(app->ren,W,H);
        SDL_SetRenderDrawBlendMode(app->ren,SDL_BLENDMODE_BLEND);
        logLine("main: renderer ok");
        app->fonts.init();
        if(app->fonts.paths[0].empty()){ logLine("font MISSING"); break; }
        logLine("main: font ok");
        for(int i=0;i<SDL_NumJoysticks();i++)
            if(SDL_IsGameController(i)&&!app->pad)app->pad=SDL_GameControllerOpen(i);
        try{ app->init(); app->run(); }
        catch(const std::exception& e){ logLine("EXCEPTION"); logLine(e.what()); }
        catch(...){ logLine("EXCEPTION unknown"); }
        gQuit=true;
        app->shutdown();
        if(app->pad){SDL_GameControllerClose(app->pad);app->pad=nullptr;}
        if(app->ren){SDL_DestroyRenderer(app->ren);app->ren=nullptr;}
        if(app->win){SDL_DestroyWindow(app->win);app->win=nullptr;}
        app.reset();
    } while(false);
    if(ttf)TTF_Quit();
    SDL_Quit();
    if(curl)curl_global_cleanup();
    if(R_SUCCEEDED(rcRomfs))romfsExit();
    sslExit(); socketExit();
    appletExit();
    if(gAccountOk)accountExit();
    return 0;
}
