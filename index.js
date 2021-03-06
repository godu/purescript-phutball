// Generated by purs bundle 0.13.6
var PS = {};
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];                 
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  exports["Nothing"] = Nothing;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Halogen.VDom.Types"] = $PS["Halogen.VDom.Types"] || {};
  var exports = $PS["Halogen.VDom.Types"];
  var Text = (function () {
      function Text(value0) {
          this.value0 = value0;
      };
      Text.create = function (value0) {
          return new Text(value0);
      };
      return Text;
  })();
  var Elem = (function () {
      function Elem(value0, value1, value2, value3) {
          this.value0 = value0;
          this.value1 = value1;
          this.value2 = value2;
          this.value3 = value3;
      };
      Elem.create = function (value0) {
          return function (value1) {
              return function (value2) {
                  return function (value3) {
                      return new Elem(value0, value1, value2, value3);
                  };
              };
          };
      };
      return Elem;
  })();
  exports["Text"] = Text;
  exports["Elem"] = Elem;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Halogen.HTML.Core"] = $PS["Halogen.HTML.Core"] || {};
  var exports = $PS["Halogen.HTML.Core"];
  var Halogen_VDom_Types = $PS["Halogen.VDom.Types"];
  var HTML = function (x) {
      return x;
  };
  var text = function ($31) {
      return HTML(Halogen_VDom_Types.Text.create($31));
  };                                
  var element = function (ns) {
      return function (name) {
          return function (props) {
              return function (children) {
                  return new Halogen_VDom_Types.Elem(ns, name, props, children);
              };
          };
      };
  };
  exports["text"] = text;
  exports["element"] = element;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Halogen.HTML.Elements"] = $PS["Halogen.HTML.Elements"] || {};
  var exports = $PS["Halogen.HTML.Elements"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var element = Halogen_HTML_Core.element(Data_Maybe.Nothing.value);
  var span = element("span");
  var span_ = span([  ]);
  var div = element("div");
  var div_ = div([  ]);
  var button = element("button");
  var button_ = button([  ]);
  exports["button_"] = button_;
  exports["div_"] = div_;
  exports["span_"] = span_;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Phutball.StaticHTML"] = $PS["Phutball.StaticHTML"] || {};
  var exports = $PS["Phutball.StaticHTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];                
  var staticHTML = Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.span_([ Halogen_HTML_Core.text("This is text in a span !") ]) ]), Halogen_HTML_Elements.button_([ Halogen_HTML_Core.text("You can click me, but I don't do anything.") ]) ]);
  exports["staticHTML"] = staticHTML;
})(PS);
PS["Phutball.StaticHTML"].main();