# pip install requests pillow
import re, json, pathlib, io, sys
import requests
from PIL import Image

BASE = "https://gtaradio.net"
OUT  = pathlib.Path("RadioNX_assets/gta")
S = requests.Session()
S.headers["User-Agent"] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/122 Safari/537.36"

def get(u):
    r = S.get(u, timeout=40); r.raise_for_status(); return r.content

def save(raw, dst, size, square=False):
    dst.parent.mkdir(parents=True, exist_ok=True)
    if dst.exists(): return
    im = Image.open(io.BytesIO(raw)).convert("RGBA")
    if square:
        w,h = im.size; s=min(w,h)
        im = im.crop(((w-s)//2,(h-s)//2,(w-s)//2+s,(h-s)//2+s))
    im.thumbnail((size,size), Image.LANCZOS)
    im.save(dst,"PNG",optimize=True)
    print("  +", dst.relative_to(OUT), im.size)

man = json.loads(get(BASE+"/build/games/manifest.json"))
print("manifest entries:", len(man))

ST = re.compile(r"resources/images/games/([^/]+)/stations/(?:covers/(\d+)/)?([^/]+)\.(png|jpg)$")
catalog = {}

for src, ent in man.items():
    file = ent.get("file")
    if not file: continue
    url = BASE + "/build/games/" + file

    m = ST.match(src)
    if m:
        game, cover, mount, ext = m.groups()
        g = catalog.setdefault(game, {})
        st = g.setdefault(mount, {"mount": mount, "name": mount, "logo": False, "cover": False})
        try:
            if cover:
                if cover != "256": continue          # берём только 256, остальные не нужны
                save(get(url), OUT/"covers"/f"{game}_{mount}.png", 256, True)
                st["cover"] = True
            else:
                save(get(url), OUT/"stations"/f"{game}_{mount}.png", 256)
                st["logo"] = True
        except Exception as e: print("  !", src, e)
        continue

    # фоны-карты и логотипы игр
    m2 = re.match(r"resources/images/games/([^/]+)/(background|logo[^/]*|.*logo[^/]*)\.png$", src)
    if m2:
        game, kind = m2.groups()
        try: save(get(url), OUT/"games"/f"{game}_{kind.replace('/','_')}.png", 512)
        except Exception as e: print("  !", src, e)
        continue

    # шрифты GTA — пригодятся для меню внутри игр
    if src.startswith("resources/fonts/") and src.endswith((".ttf",".otf")):
        try:
            dst = OUT/"fonts"/pathlib.Path(src).name
            dst.parent.mkdir(parents=True, exist_ok=True)
            if not dst.exists(): dst.write_bytes(get(url)); print("  + font", dst.name)
        except Exception as e: print("  !", src, e)
        continue

    # звуки переключения станций
    if src.startswith("resources/sounds/"):
        try:
            dst = OUT/"sfx"/pathlib.Path(src).name
            dst.parent.mkdir(parents=True, exist_ok=True)
            if not dst.exists(): dst.write_bytes(get(url)); print("  + sfx", dst.name)
        except Exception as e: print("  !", src, e)

# JS игр: вытаскиваем реальные имена станций и URL потоков (в т.ч. HLS для 1/2/london/advance/ctw)
(OUT/"_dump").mkdir(parents=True, exist_ok=True)
for src, ent in man.items():
    m = re.match(r"resources/js/games/([a-z0-9]+)\.js$", src)
    if not m: continue
    game = m.group(1)
    try: js = get(BASE+"/build/games/"+ent["file"]).decode("utf-8","replace")
    except Exception as e: print("  ! js", game, e); continue
    (OUT/"_dump"/f"{game}.js").write_text(js, "utf-8")
    streams = sorted(set(re.findall(r'https?://[^\s"\'`]+?\.(?:m3u8|mp3|aac)[^\s"\'`]*', js)))
    pairs   = re.findall(r'\{[^{}]*?(?:name|title)\s*:\s*["\'`]([^"\'`]{2,48})["\'`][^{}]*?\}', js)
    if streams or pairs:
        print(f"[{game}] streams={len(streams)} names={len(pairs)}")
        (OUT/"_dump"/f"{game}.txt").write_text(
            "\n".join(streams)+"\n---\n"+"\n".join(pairs), "utf-8")
    g = catalog.setdefault(game, {})
    for nm in pairs:
        slug = re.sub(r"[^a-z0-9]+","-",nm.lower()).strip("-")
        g.setdefault(slug, {"mount":slug,"name":nm,"logo":False,"cover":False})

# главные обложки игр (4 штуки из основного манифеста)
try:
    main = json.loads(get(BASE+"/build/manifest.json"))
    for src, ent in main.items():
        m = re.match(r"resources/images/games/gta-([a-z0-9]+)\.jpg$", src)
        if m: save(get(BASE+"/build/"+ent["file"]), OUT/"games"/f"{m.group(1)}.png", 384, True)
except Exception as e: print("main manifest:", e)

LABEL = {"4":"GTA IV","sa":"GTA San Andreas","vc":"GTA Vice City","vcs":"GTA Vice City Stories",
         "lcs":"GTA Liberty City Stories","3":"GTA III","ctw":"GTA Chinatown Wars",
         "advance":"GTA Advance","2":"GTA 2","london":"GTA London","1":"GTA 1"}
HLS = {"1","2","london","advance","ctw"}
out = {"source":"gtaradio.net","games":[]}
for game in LABEL:
    sts = sorted(catalog.get(game,{}).values(), key=lambda s:s["name"])
    for s in sts:
        s["url"] = "" if game in HLS else f"https://audio.gtaradio.net/{game}/{s['mount']}"
        s["hls"] = game in HLS
    out["games"].append({"key":game,"label":LABEL[game],"stations":sts})
    print(f"{game:8s} {len(sts):3d} stations, logos={sum(s['logo'] for s in sts)}")
(OUT/"catalog.json").write_text(json.dumps(out,ensure_ascii=False,indent=1),"utf-8")
print("\nDONE ->", OUT.resolve())