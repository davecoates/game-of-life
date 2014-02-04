goog.addDependency("base.js", ['goog'], []);
goog.addDependency("../cljs/core.js", ['cljs.core'], ['goog.string', 'goog.array', 'goog.object', 'goog.string.StringBuffer']);
goog.addDependency("../clojure/string.js", ['clojure.string'], ['cljs.core', 'goog.string', 'goog.string.StringBuffer']);
goog.addDependency("../game_of_life/patterns.js", ['game_of_life.patterns'], ['cljs.core', 'clojure.string']);
goog.addDependency("../game_of_life/core.js", ['game_of_life.core'], ['cljs.core', 'game_of_life.patterns', 'clojure.string']);