import asyncio
from playwright.async_api import async_playwright
INIT="window.__v=[];document.addEventListener('securitypolicyviolation',e=>window.__v.push(e.effectiveDirective+' '+e.blockedURI.slice(0,12)));"
async def one(p,name,port,label):
  b=await getattr(p,name).launch(); ctx=await b.new_context(viewport={'width':1440,'height':1000}); await ctx.add_init_script(INIT)
  pg=await ctx.new_page(); await pg.goto(f'http://127.0.0.1:{port}/pdfjs/web/viewer.html?file=/cv.pdf'); await pg.wait_for_timeout(2500)
  thumbs=[]
  await pg.evaluate("window.__pc=0; new MutationObserver(()=>{const im=[...document.querySelectorAll('#printContainer img')]; window.__pc=Math.max(window.__pc,im.length); window.__pl=Math.max(window.__pl||0, im.filter(i=>i.complete&&i.naturalWidth>0).length)}).observe(document.body,{subtree:true,childList:true}); setTimeout(()=>window.print(),0)"); await pg.wait_for_timeout(5000)
  v=await pg.evaluate("window.__v"); pc=await pg.evaluate("[window.__pc, window.__pl||0, document.querySelectorAll('#printContainer img').length, [...document.querySelectorAll('#printContainer img')].filter(i=>i.complete&&i.naturalWidth>0).length]")
  print(f'{label:4} {name:8} thumbs(loaded)={sum(thumbs)}/{len(thumbs)} print-imgs(max)={pc} violations={sorted(set(v))} x{len(v)}'); await b.close()
async def main():
  async with async_playwright() as p:
    for name in ('chromium','firefox'):
      await one(p,name,48753,'OLD'); await one(p,name,48752,'NEW')
asyncio.run(main())
