//
// Page modes

require('gmail.js');
require('google-search-results.js');
require('google-reader.js')

define_key(google_reader_keymap, "D", null, $fallthrough);
define_key(google_reader_keymap, "g", null, $fallthrough);
define_key(google_reader_keymap, "f", null, $fallthrough);
define_key(google_reader_keymap, "F", null, $fallthrough);
google_search_bind_number_shortcuts();

//
// Variables

//set_user_agent("Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.4)" +
//               " Gecko/20100614 Firefox/3.6.4");
url_completion_use_history = true;
session_pref('browser.history_expire_days', 7);
session_pref('browser.history_expire_days_min', 5);
url_remoting_fn = load_url_in_new_buffer;

//
// Commands

interactive("llasram/follow", null,
    alternates(follow, follow_new_buffer, follow_new_buffer_background),
    $browser_object = browser_object_links);

interactive("llasram/caret-copy", null,
            function (I) {
              if (I.buffer.mark_active) {
                I.buffer.do_command("cmd_copy");
                clear_selection(I.buffer);
                I.buffer.mark_active = false;
                caret_mode(I.buffer, false);
              }
            });

//
// Hooks

function caret_clear_mark_selection(buffer) {
  clear_selection(buffer);
  buffer.mark_active = false;
}
add_hook('caret_mode_disable_hook', caret_clear_mark_selection);

//
// Key bindings

modifiers.M = new modifier(
    function (event) { return event.metaKey; },
    function (event) { event.metaKey = true; });

undefine_key(content_buffer_normal_keymap, "C-h");
undefine_key(default_global_keymap, "C-h");
undefine_key(sequence_help_keymap, "C-h");
define_key(default_global_keymap, "C-c h", default_help_keymap);

define_key(default_global_keymap, "C-[", "caret-mode");
define_key(default_global_keymap, "C-t c", "caret-mode");
define_key(content_buffer_text_keymap, "C-t c", "caret-mode");
define_key(default_global_keymap, "C-t C-c", "caret-mode");
define_key(content_buffer_text_keymap, "C-t C-c", "caret-mode");
define_key(caret_keymap, "M-w", "llasram/caret-copy");
define_key(caret_keymap, "C-g", "caret-mode");

define_key(default_global_keymap, "M-<", "cmd_scrollTop");
define_key(content_buffer_normal_keymap, "M-<", "cmd_scrollTop");
define_key(default_global_keymap, "home", "cmd_scrollTop");
define_key(content_buffer_normal_keymap, "home", "cmd_scrollTop");

define_key(text_keymap, "C-h", "cmd_deleteCharBackward");
define_key(text_keymap, "M-h", "cmd_deleteWordBackward");

define_key(content_buffer_text_keymap, "C-g", "unfocus");

define_key(content_buffer_normal_keymap, "b", "back");
define_key(default_global_keymap, "M-b", "back");
define_key(default_global_keymap, "C-t b", "back");
define_key(content_buffer_text_keymap, "C-t b", "back");
define_key(default_global_keymap, "C-t C-b", "back");
define_key(content_buffer_text_keymap, "C-t C-b", "back");
define_key(content_buffer_normal_keymap, "f", "forward");
define_key(default_global_keymap, "M-f", "forward");
define_key(default_global_keymap, "C-t f", "forward");
define_key(content_buffer_text_keymap, "C-t f", "forward");
define_key(default_global_keymap, "C-t C-f", "forward");
define_key(content_buffer_text_keymap, "C-t C-f", "forward");

define_key(default_global_keymap, "C-M-b", "bookmark");

define_key(content_buffer_normal_keymap, "i", "find-url");
define_key(content_buffer_normal_keymap, "I", "find-alternate-url");
define_key(default_global_keymap, "C-l", "find-alternate-url");
define_key(content_buffer_text_keymap, "C-l", "find-alternate-url");
define_key(default_global_keymap, "C-t C-l", "find-url-new-buffer");
define_key(content_buffer_text_keymap, "C-t C-l", "find-url-new-buffer");
define_key(default_global_keymap, "C-t l", "find-url-new-buffer");
define_key(content_buffer_text_keymap, "C-t l", "find-url-new-buffer");
define_key(default_global_keymap, "C-t i", "find-url");
define_key(content_buffer_text_keymap, "C-t i", "find-url");
define_key(default_global_keymap, "C-t C-i", "find-url");
define_key(content_buffer_text_keymap, "C-t C-i", "find-url");

define_key(content_buffer_normal_keymap, "u", "llasram/follow");
define_key(default_global_keymap, "C-t u", "llasram/follow");
define_key(content_buffer_text_keymap, "C-t u", "llasram/follow");
define_key(default_global_keymap, "C-t C-u", "llasram/follow");
define_key(content_buffer_text_keymap, "C-t C-u", "llasram/follow");

define_key(default_global_keymap, "C-tab", "buffer-next");
define_key(content_buffer_text_keymap, "C-tab", "buffer-next");
define_key(default_global_keymap, "C-S-tab", "buffer-previous");
define_key(content_buffer_text_keymap, "C-S-tab", "buffer-previous");

//
// Webjumps

define_webjump("amazon", "http://www.amazon.com/exec/obidos/external-search/?field-keywords=%s&mode=blended");
define_webjump("mw", "http://www.merriam-webster.com/dictionary/%s");
define_webjump("bn", "http://productsearch.barnesandnoble.com/search/results.aspx?store=EBOOK&WRD=%s");

define_webjump("areader", "javascript:var%20b=document.body;var%20GR________bookmarklet_domain='http://www.google.com';if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.src='http://www.google.com/reader/ui/link-bookmarklet.js');void(b.appendChild(z));}else{}");

define_webjump("aboxee", "javascript:var%20b=document.body;if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.src='http://www.boxee.tv/queue/watchlaterloader?'+(new%20Date()).getTime().toString());void(b.appendChild(z));}else{}");

define_webjump("aevernote", "javascript:(function(){EN_CLIP_HOST='http://www.evernote.com';try{var%20x=document.createElement('SCRIPT');x.type='text/javascript';x.src=EN_CLIP_HOST+'/public/bookmarkClipper.js?'+(new%20Date().getTime()/100000);document.getElementsByTagName('head')[0].appendChild(x);}catch(e){location.href=EN_CLIP_HOST+'/clip.action?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);}})();");

define_webjump("apocket", "javascript:(function(){var%20e=function(t,n,r,i,s){var%20o=[6177815,5755621,4498421,4056017,1653452,2878209,3762641,5467934,2475508,1598829];var%20i=i||0,u=0,n=n||[],r=r||0,s=s||0;var%20a={'a':97,'b':98,'c':99,'d':100,'e':101,'f':102,'g':103,'h':104,'i':105,'j':106,'k':107,'l':108,'m':109,'n':110,'o':111,'p':112,'q':113,'r':114,'s':115,'t':116,'u':117,'v':118,'w':119,'x':120,'y':121,'z':122,'A':65,'B':66,'C':67,'D':68,'E':69,'F':70,'G':71,'H':72,'I':73,'J':74,'K':75,'L':76,'M':77,'N':78,'O':79,'P':80,'Q':81,'R':82,'S':83,'T':84,'U':85,'V':86,'W':87,'X':88,'Y':89,'Z':90,'0':48,'1':49,'2':50,'3':51,'4':52,'5':53,'6':54,'7':55,'8':56,'9':57,'\/':47,':':58,'?':63,'=':61,'-':45,'_':95,'&':38,'$':36,'!':33,'.':46};if(!s||s==0){t=o[0]+t}for(var%20f=0;f<t.length;f++){var%20l=function(e,t){return%20a[e[t]]?a[e[t]]:e.charCodeAt(t)}(t,f);if(!l*1)l=3;var%20c=l*(o[i]+l*o[u%o.length]);n[r]=(n[r]?n[r]+c:c)+s+u;var%20p=c%(50*1);if(n[p]){var%20d=n[r];n[r]=n[p];n[p]=d}u+=c;r=r==50?0:r+1;i=i==o.length-1?0:i+1}if(s==318){var%20v='';for(var%20f=0;f<n.length;f++){v+=String.fromCharCode(n[f]%(25*1)+97)}o=function(){};return%20v+'4d630da02c'}else{return%20e(u+'',n,r,i,s+1)}};var%20t=document,n=t.location.href,r=t.title;var%20i=e(n);var%20s=t.createElement('script');s.type='text/javascript';s.src='https://getpocket.com/b/r4.js?h='+i+'&u='+encodeURIComponent(n)+'&t='+encodeURIComponent(r);e=i=function(){};var%20o=t.getElementsByTagName('head')[0]||t.documentElement;o.appendChild(s)})()");

define_webjump("apinboard", "javascript:if(document.getSelection){s=document.getSelection();}else{s='';};document.location='https://pinboard.in/add?next=same&url='+encodeURIComponent(location.href)+'&description='+encodeURIComponent(s)+'&title='+encodeURIComponent(document.title)");

define_webjump(
    "wb", "http://web.archive.org/web/*/%s",
    $alternative = ("javascript:window.location.href=" +
                    "'http://web.archive.org/web/*/'" +
                    "+window.location.href;"));

define_webjump("vtdn", "https://www.virustotal.com/en/domain/%s/information/");

webjumps.g = webjumps.google;
webjumps.gl = webjumps.lucky;
webjumps.a = webjumps.amazon;
