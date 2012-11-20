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

function alias_webjump(alias, name) {
  var orig = webjumps[name];
  var wj = {};
  for (var key in orig) {
    wj[key] = orig[key];
  }
  wj.key = alias;
  webjumps[alias] = wj;
}

add_delicious_webjumps("llasram");

define_webjump("amazon", "http://www.amazon.com/exec/obidos/external-search/?field-keywords=%s&mode=blended");
define_webjump("mw", "http://www.merriam-webster.com/dictionary/%s");
define_webjump("bn", "http://productsearch.barnesandnoble.com/search/results.aspx?store=EBOOK&WRD=%s");

define_webjump("wb", function (url) {
    if (url) {
      return "http://web.archive.org/web/*/" + url;
    } else {
      return "javascript:window.location.href=" +
        "'http://web.archive.org/web/*/'+window.location.href;";
    }
  },
  $argument="optional"
);

define_webjump("areader", "javascript:var%20b=document.body;var%20GR________bookmarklet_domain='http://www.google.com';if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.src='http://www.google.com/reader/ui/link-bookmarklet.js');void(b.appendChild(z));}else{}");

define_webjump("aboxee", "javascript:var%20b=document.body;if(b&&!document.xmlVersion){void(z=document.createElement('script'));void(z.src='http://www.boxee.tv/queue/watchlaterloader?'+(new%20Date()).getTime().toString());void(b.appendChild(z));}else{}");

alias_webjump('g', 'google');
alias_webjump('gl', 'lucky');
alias_webjump('d', 'delicious');
alias_webjump('wp', 'wikipedia');
alias_webjump('a', 'amazon');

read_url_handler_list = [read_url_make_default_webjump_handler("g")];

//
// Save passwords

session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");

Components.classes["@mozilla.org/login-manager;1"]
    .getService(Components.interfaces.nsILoginManager);
