var pas = { $libimports: {}};

var rtl = {

  version: 20200,

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  $res : {},

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  checkVersion: function(v){
    if (rtl.version != v) throw "expected rtl version "+v+", but found "+rtl.version;
  },

  hiInt: Math.pow(2,53),

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isTRecord: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$new') && (typeof(type.$new)==='function'));
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist);
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var r = Object.create(rtl.tSectionRTTI);
    var module = r.$module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: null,
      $impl: null,
      $rtti: r
    };
    if (impluseslist) module.$impl = {
          $module: module,
          $rtti: r
        };
  },

  exitcode: 0,

  run: function(module_name){
    try {
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if ((module_name=='program') || (module_name=='library')){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas[module_name].$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    } catch(re) {
      if (!rtl.showUncaughtExceptions) {
        throw re
      } else {  
        if (!rtl.handleUncaughtException(re)) {
          rtl.showException(re);
          rtl.exitcode = 216;
        }  
      }
    } 
    return rtl.exitcode;
  },
  
  showException : function (re) {
    var errMsg = rtl.hasString(re.$classname) ? re.$classname : '';
    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
    alert('Uncaught Exception : '+errMsg);
  },

  handleUncaughtException: function (e) {
    if (rtl.onUncaughtException) {
      try {
        rtl.onUncaughtException(e);
        return true;
      } catch (ee) {
        return false; 
      }
    } else {
      return false;
    }
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    var len = useslist.length;
    for (var i = 0; i<len; i++) {
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  createSafeCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      if (!scope.hasOwnProperty('$events')) scope.$events = {};
      cb = scope.$events[fn];
      if (cb) return cb;
      scope.$events[fn] = cb = function(){
        try{
          return scope[fn].apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    } else {
      cb = function(){
        try{
          return fn.apply(scope,arguments);
        } catch (err) {
          if (!rtl.handleUncaughtException(err)) throw err;
        }
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a===b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn===b.fn);
    }
  },

  initStruct: function(c,parent,name){
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.$name+'.'+name;
    };
    return parent;
  },

  initClass: function(c,parent,name,initfn,rttiname){
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$classname,{ "class": c });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn,rttiname){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = { $ancestor: null };
      c.$create = function(fn,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$init();
        try{
          if (typeof(fn)==="string"){
            o[fn].apply(o,args);
          } else {
            fn.apply(o,args);
          };
          o.AfterConstruction();
        } catch($e){
          // do not call BeforeDestruction
          if (o.Destroy) o.Destroy();
          o.$final();
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        if (this[fnname]) this[fnname]();
        this.$final();
      };
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn,rttiname){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var isFunc = rtl.isFunction(ancestor);
    var c = null;
    if (isFunc){
      // create pascal class descendent from JS function
      c = Object.create(ancestor.prototype);
      c.$ancestorfunc = ancestor;
      c.$ancestor = null; // no pascal ancestor
    } else if (ancestor.$func){
      // create pascal class descendent from a pascal class descendent of a JS function
      isFunc = true;
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
    } else {
      c = Object.create(ancestor);
      c.$ancestor = null; // no pascal ancestor
    }
    c.$create = function(fn,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fn,args);
      } else if(isFunc) {
        o = new this.$func(args);
      } else {
        o = Object.create(c);
      }
      if (o.$init) o.$init();
      try{
        if (typeof(fn)==="string"){
          this[fn].apply(o,args);
        } else {
          fn.apply(o,args);
        };
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        // do not call BeforeDestruction
        if (o.Destroy) o.Destroy();
        if (o.$final) o.$final();
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      if (this[fnname]) this[fnname]();
      if (this.$final) this.$final();
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
    if (isFunc){
      function f(){}
      f.prototype = c;
      c.$func = f;
    }
  },

  createHelper: function(parent,name,ancestor,initfn,rttiname){
    // create a helper,
    // ancestor must be null or a helper,
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // c.$ancestor === Object.getPrototypeOf(c)
    } else {
      c = { $ancestor: null };
    };
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    var t = c.$module.$rtti.$Helper(c.$classname,{ "helper": c });
    c.$rtti = t;
    if (rtl.isObject(ancestor)) t.ancestor = ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return null;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return null;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  hideProp: function(o,p,v){
    Object.defineProperty(o,p, {
      enumerable: false,
      configurable: true,
      writable: true
    });
    if(arguments.length>2){ o[p]=v; }
  },

  recNewT: function(parent,name,initfn,full){
    // create new record type
    var t = {};
    if (parent) parent[name] = t;
    var h = rtl.hideProp;
    if (full){
      rtl.initStruct(t,parent,name);
      t.$record = t;
      h(t,'$record');
      h(t,'$name');
      h(t,'$parent');
      h(t,'$module');
      h(t,'$initSpec');
    }
    initfn.call(t);
    if (!t.$new){
      t.$new = function(){ return Object.create(t); };
    }
    t.$clone = function(r){ return t.$new().$assign(r); };
    h(t,'$new');
    h(t,'$clone');
    h(t,'$eq');
    h(t,'$assign');
    return t;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,
  EIntOverflow: null,
  EPropWriteOnly: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = name;
    i.$fullname = module.$name+'.'+name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,TGuid.$new());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    if (!aclass.hasOwnProperty('$intfmaps')) aclass.$intfmaps = {};
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsIntfT: function(intf,intftype){
    return (intf!==null) && rtl.queryIntfIsT(intf.$o,intftype);
  },

  intfAsIntfT: function (intf,intftype){
    if (!intf) return null;
    var i = rtl.getIntfG(intf.$o,intftype.$guid);
    if (i) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      if(intf) {
        this[id]=intf;
      }
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          var intf = this[id];
          if (intf){
            //console.log('rtl.intfRefs.free: id='+id+' '+intf.$name+' $o='+intf.$o.$classname);
            intf._Release();
          }
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  trunc: function(a){
    return a<0 ? Math.ceil(a) : Math.floor(a);
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  oc: function(i){
    // overflow check integer
    if ((Math.floor(i)===i) && (i>=-0x1fffffffffffff) && (i<=0x1fffffffffffff)) return i;
    rtl.raiseE('EIntOverflow');
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arrayRef: function(a){
    if (a!=null) rtl.hideProp(a,'$pas2jsrefcnt',1);
    return a;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    var stack = [];
    var s = 9999;
    for (var i=2; i<arguments.length; i++){
      var j = arguments[i];
      if (j==='s'){ s = i-2; }
      else {
        stack.push({ dim:j+0, a:null, i:0, src:null });
      }
    }
    var dimmax = stack.length-1;
    var depth = 0;
    var lastlen = 0;
    var item = null;
    var a = null;
    var src = arr;
    var srclen = 0, oldlen = 0;
    do{
      if (depth>0){
        item=stack[depth-1];
        src = (item.src && item.src.length>item.i)?item.src[item.i]:null;
      }
      if (!src){
        a = [];
        srclen = 0;
        oldlen = 0;
      } else if (src.$pas2jsrefcnt>0 || depth>=s){
        a = [];
        srclen = src.length;
        oldlen = srclen;
      } else {
        a = src;
        srclen = 0;
        oldlen = a.length;
      }
      lastlen = stack[depth].dim;
      a.length = lastlen;
      if (depth>0){
        item.a[item.i]=a;
        item.i++;
        if ((lastlen===0) && (item.i<item.a.length)) continue;
      }
      if (lastlen>0){
        if (depth<dimmax){
          item = stack[depth];
          item.a = a;
          item.i = 0;
          item.src = src;
          depth++;
          continue;
        } else {
          if (srclen>lastlen) srclen=lastlen;
          if (rtl.isArray(defaultvalue)){
            // array of dyn array
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=[];
          } else if (rtl.isObject(defaultvalue)) {
            if (rtl.isTRecord(defaultvalue)){
              // array of record
              for (var i=0; i<srclen; i++) a[i]=defaultvalue.$clone(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue.$new();
            } else {
              // array of set
              for (var i=0; i<srclen; i++) a[i]=rtl.refSet(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]={};
            }
          } else {
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue;
          }
        }
      }
      // backtrack
      while ((depth>0) && (stack[depth-1].i>=stack[depth-1].dim)){
        depth--;
      };
      if (depth===0){
        if (dimmax===0) return a;
        return stack[0].a;
      }
    }while (true);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    } else if (rtl.isTRecord(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = type.$clone(src[srcpos]); // clone record
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=0; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=rtl.arrayRef(src); // Note: concat(a) does not clone
      } else {
        a=a.concat(src);
      }
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  arrayInsert: function(item, arr, index){
    if (arr){
      arr.splice(index,0,item);
      return arr;
    } else {
      return [item];
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    rtl.hideProp(s,'$shared',true);
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
      return s;
    };
  },

  floatToStr: function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (((ad>1) && (ad<1.0e+10)) ||  ((ad>1.e-10) && (ad<1))) {
		pad='00';
	  } else if ((ad>1) && (ad<1.0e+100) || (ad<1.e-10)) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=24;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  valEnum: function(s, enumType, setCodeFn){
    s = s.toLowerCase();
    for (var key in enumType){
      if((typeof(key)==='string') && (key.toLowerCase()===s)){
        setCodeFn(0);
        return enumType[key];
      }
    }
    setCodeFn(1);
    return 0;
  },

  lw: function(l){
    // fix longword bitwise operation
    return l<0?l+0x100000000:l;
  },

  and: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) & (b / hi);
    var l = (a & low) & (b & low);
    return h*hi + l;
  },

  or: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) | (b / hi);
    var l = (a & low) | (b & low);
    return h*hi + l;
  },

  xor: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) ^ (b / hi);
    var l = (a & low) ^ (b & low);
    return h*hi + l;
  },

  shr: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (a<0x80000000) return a >> b;
    if (b<=0) return a;
    if (b>54) return 0;
    return Math.floor(a / Math.pow(2,b));
  },

  shl: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (b<=0) return a;
    if (b>54) return 0;
    var r = a * Math.pow(2,b);
    if (r <= rtl.hiInt) return r;
    return r % rtl.hiInt;
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo", kind: 0, $module: null, attr: null };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = { attr: null };
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,flags,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params,result,flags);
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      t.params = rtl.isArray(t.params) ? rtl.newTIParams(t.params) : null;
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",18 /* tkInterface */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoHelper",19 /* tkHelper */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoExtClass",20 /* tkExtClass */,rtl.tTypeInfoClass);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); },
    $Helper: function(name,o){ return this.$Scope(name,rtl.tTypeInfoHelper,o); },
    $ExtClass: function(name,o){ return this.$Scope(name,rtl.tTypeInfoExtClass,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result?result:null,
      flags: flags?flags:0
    };
    return s;
  },

  addResource: function(aRes){
    rtl.$res[aRes.name]=aRes;
  },

  getResource: function(aName){
    var res = rtl.$res[aName];
    if (res !== undefined) {
      return res;
    } else {
      return null;
    }
  },

  getResourceList: function(){
    return Object.keys(rtl.$res);
  }
}

rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  this.LineEnding = "\n";
  this.sLineBreak = this.LineEnding;
  this.MaxLongint = 0x7fffffff;
  this.Maxint = 2147483647;
  $lt = this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  rtl.createClass(this,"TObject",null,function () {
    $lt1 = this;
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.FieldAddress = function (aName) {
      var Result = null;
      Result = null;
      if (aName === "") return Result;
      var aClass = this.$class;
      var ClassTI = null;
      var myName = aName.toLowerCase();
      var MemberTI = null;
      while (aClass !== null) {
        ClassTI = aClass.$rtti;
        for (var i = 0, $end2 = ClassTI.fields.length - 1; i <= $end2; i++) {
          MemberTI = ClassTI.getField(i);
          if (MemberTI.name.toLowerCase() === myName) {
             return MemberTI;
          };
        };
        aClass = aClass.$ancestor ? aClass.$ancestor : null;
      };
      return Result;
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
  });
  this.vtInteger = 0;
  this.vtExtended = 3;
  this.vtWideChar = 9;
  this.vtCurrency = 12;
  this.vtUnicodeString = 18;
  this.vtNativeInt = 19;
  rtl.recNewT(this,"TVarRec",function () {
    $lt2 = this;
    this.VType = 0;
    this.VJSValue = undefined;
    this.$eq = function (b) {
      return (this.VType === b.VType) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue) && (this.VJSValue === b.VJSValue);
    };
    this.$assign = function (s) {
      this.VType = s.VType;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      this.VJSValue = s.VJSValue;
      return this;
    };
  });
  this.VarRecs = function () {
    var Result = [];
    var i = 0;
    var v = null;
    Result = [];
    while (i < arguments.length) {
      v = $lt2.$new();
      v.VType = rtl.trunc(arguments[i]);
      i += 1;
      v.VJSValue = arguments[i];
      i += 1;
      Result.push($lt2.$clone(v));
    };
    return Result;
  };
  this.IsConsole = false;
  this.OnParamCount = null;
  this.OnParamStr = null;
  this.Frac = function (A) {
    return A % 1;
  };
  this.Odd = function (A) {
    return A&1 != 0;
  };
  this.Sqr$1 = function (A) {
    return A*A;
  };
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.DefaultTextLineBreakStyle = 0;
  this.Int = function (A) {
    var Result = 0.0;
    Result = $mod.Trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if ((Index < 1) || (Index > S.get().length) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set($mod.Copy(t,1,Index - 1) + Insertion + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    NI.set($impl.valint(S,-9007199254740991,9007199254740991,Code));
  };
  this.val$6 = function (S, I, Code) {
    I.set($impl.valint(S,-2147483648,2147483647,Code));
  };
  this.val$8 = function (S, d, Code) {
    var x = 0.0;
    if (S === "") {
      Code.set(1);
      return;
    };
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else {
      Code.set(0);
      d.set(x);
    };
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
    Result = "";
    for (var $l = 1, $end = l; $l <= $end; $l++) {
      i = $l;
      Result = Result + c;
    };
    return Result;
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = arguments.length - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l = 0, $end = l; $l <= $end; $l++) {
        i = $l;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l1 = 0, $end1 = l; $l1 <= $end1; $l1++) {
        i = $l1;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  this.SetWriteCallBack = function (H) {
    var Result = null;
    Result = $impl.WriteCallBack;
    $impl.WriteCallBack = H;
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  $mod.$implcode = function () {
    $impl.WriteBuf = "";
    $impl.WriteCallBack = null;
    $impl.valint = function (S, MinVal, MaxVal, Code) {
      var Result = 0;
      var x = 0.0;
      if (S === "") {
        Code.set(1);
        return Result;
      };
      x = Number(S);
      if (isNaN(x)) {
        var $tmp = $mod.Copy(S,1,1);
        if ($tmp === "$") {
          x = Number("0x" + $mod.Copy$1(S,2))}
         else if ($tmp === "&") {
          x = Number("0o" + $mod.Copy$1(S,2))}
         else if ($tmp === "%") {
          x = Number("0b" + $mod.Copy$1(S,2))}
         else {
          Code.set(1);
          return Result;
        };
      };
      if (isNaN(x) || (x !== $mod.Int(x))) {
        Code.set(1)}
       else if ((x < MinVal) || (x > MaxVal)) {
        Code.set(2)}
       else {
        Result = $mod.Trunc(x);
        Code.set(0);
      };
      return Result;
    };
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},[]);
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  $lt = this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  $lt1 = this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lm = pas.System;
  var $lt8 = $lm.TObject;
  rtl.createClass(this,"EJS",$lt8,function () {
    $lt = this;
    this.$init = function () {
      $lt8.$init.call(this);
      this.FMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.FMessage = Msg;
      return this;
    };
  });
  this.New = function (aElements) {
    var Result = null;
    var L = 0;
    var I = 0;
    var S = "";
    L = rtl.length(aElements);
    if ((L % 2) === 1) throw $lt.$create("Create$1",["Number of arguments must be even"]);
    I = 0;
    while (I < L) {
      if (!rtl.isString(aElements[I])) {
        S = String(I);
        throw $lt.$create("Create$1",["Argument " + S + " must be a string."]);
      };
      I += 2;
    };
    I = 0;
    Result = new Object();
    while (I < L) {
      S = "" + aElements[I];
      Result[S] = aElements[I + 1];
      I += 2;
    };
    return Result;
  };
  this.JSDelete = function (Obj, PropName) {
    return delete Obj[PropName];
  };
  this.isBoolean = function (v) {
    return typeof(v) == 'boolean';
  };
  this.isDate = function (v) {
    return (v instanceof Date);
  };
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.isUndefined = function (v) {
    return v == undefined;
  };
  this.isDefined = function (v) {
    return !(v == undefined);
  };
  this.toNumber = function (v) {
    return v-0;
  };
});
rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lt15 = null;
  var $lt16 = null;
  var $lt17 = null;
  var $lt18 = null;
  var $lt19 = null;
  var $lt20 = null;
  var $lt21 = null;
  var $lt22 = null;
  var $lt23 = null;
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SArgumentMissing: {org: 'Missing argument in format "%s"'}, SInvalidFormat: {org: 'Invalid format specifier : "%s"'}, SInvalidArgIndex: {org: 'Invalid argument index in format: "%s"'}, SListCapacityError: {org: "List capacity (%s) exceeded."}, SListCountError: {org: "List count (%s) out of bounds."}, SListIndexError: {org: "List index (%s) out of bounds"}, SSortedListError: {org: "Operation not allowed on sorted list"}, SDuplicateString: {org: "String list does not allow duplicates"}, SErrFindNeedsSortedList: {org: "Cannot use find on unsorted list"}, SInvalidName: {org: 'Invalid component name: "%s"'}, SInvalidBoolean: {org: '"%s" is not a valid boolean.'}, SDuplicateName: {org: 'Duplicate component name: "%s"'}, SErrInvalidDate: {org: 'Invalid date: "%s"'}, SErrInvalidTimeFormat: {org: 'Invalid time format: "%s"'}, SInvalidDateFormat: {org: 'Invalid date format: "%s"'}, SErrInvalidInteger: {org: 'Invalid integer value: "%s"'}, SErrInvalidFloat: {org: 'Invalid floating-point value: "%s"'}, SInvalidCurrency: {org: "Invalid currency value: %s"}};
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lm = pas.System;
  var $lt9 = $lm.TObject;
  var $lp = $lm.Writeln;
  var $lp1 = $lm.val;
  var $lp2 = $lm.Copy;
  var $lp3 = $lm.upcase;
  var $lp4 = $lm.StringOfChar;
  var $lp5 = $lm.Insert;
  var $lm1 = pas.RTLConsts;
  var $lp6 = $lm.VarRecs;
  var $lp7 = $lm.val$6;
  var $lp8 = $lm.val$8;
  var $lp9 = $lm.Trunc;
  var $lp10 = $lm.Frac;
  var $lp11 = $lm.Pos;
  var $lp12 = $lm.Delete;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  this.FloatRecDigits = 19;
  rtl.recNewT(this,"TFloatRec",function () {
    $lt = this;
    this.Exponent = 0;
    this.Negative = false;
    this.$new = function () {
      var r = Object.create(this);
      r.Digits = rtl.arraySetLength(null,"",19);
      return r;
    };
    this.$eq = function (b) {
      return (this.Exponent === b.Exponent) && (this.Negative === b.Negative) && rtl.arrayEq(this.Digits,b.Digits);
    };
    this.$assign = function (s) {
      this.Exponent = s.Exponent;
      this.Negative = s.Negative;
      this.Digits = s.Digits.slice(0);
      return this;
    };
  });
  rtl.recNewT(this,"TFormatSettings",function () {
    $lt1 = this;
    this.CurrencyDecimals = 0;
    this.CurrencyFormat = 0;
    this.CurrencyString = "";
    this.DateSeparator = "";
    this.DecimalSeparator = "";
    this.LongDateFormat = "";
    this.LongTimeFormat = "";
    this.NegCurrFormat = 0;
    this.ShortDateFormat = "";
    this.ShortTimeFormat = "";
    this.ThousandSeparator = "";
    this.TimeAMString = "";
    this.TimePMString = "";
    this.TimeSeparator = "";
    this.TwoDigitYearCenturyWindow = 0;
    this.InitLocaleHandler = null;
    this.$new = function () {
      var r = Object.create(this);
      r.DateTimeToStrFormat = rtl.arraySetLength(null,"",2);
      r.LongDayNames = rtl.arraySetLength(null,"",7);
      r.LongMonthNames = rtl.arraySetLength(null,"",12);
      r.ShortDayNames = rtl.arraySetLength(null,"",7);
      r.ShortMonthNames = rtl.arraySetLength(null,"",12);
      return r;
    };
    this.$eq = function (b) {
      return (this.CurrencyDecimals === b.CurrencyDecimals) && (this.CurrencyFormat === b.CurrencyFormat) && (this.CurrencyString === b.CurrencyString) && (this.DateSeparator === b.DateSeparator) && rtl.arrayEq(this.DateTimeToStrFormat,b.DateTimeToStrFormat) && (this.DecimalSeparator === b.DecimalSeparator) && (this.LongDateFormat === b.LongDateFormat) && rtl.arrayEq(this.LongDayNames,b.LongDayNames) && rtl.arrayEq(this.LongMonthNames,b.LongMonthNames) && (this.LongTimeFormat === b.LongTimeFormat) && (this.NegCurrFormat === b.NegCurrFormat) && (this.ShortDateFormat === b.ShortDateFormat) && rtl.arrayEq(this.ShortDayNames,b.ShortDayNames) && rtl.arrayEq(this.ShortMonthNames,b.ShortMonthNames) && (this.ShortTimeFormat === b.ShortTimeFormat) && (this.ThousandSeparator === b.ThousandSeparator) && (this.TimeAMString === b.TimeAMString) && (this.TimePMString === b.TimePMString) && (this.TimeSeparator === b.TimeSeparator) && (this.TwoDigitYearCenturyWindow === b.TwoDigitYearCenturyWindow);
    };
    this.$assign = function (s) {
      this.CurrencyDecimals = s.CurrencyDecimals;
      this.CurrencyFormat = s.CurrencyFormat;
      this.CurrencyString = s.CurrencyString;
      this.DateSeparator = s.DateSeparator;
      this.DateTimeToStrFormat = s.DateTimeToStrFormat.slice(0);
      this.DecimalSeparator = s.DecimalSeparator;
      this.LongDateFormat = s.LongDateFormat;
      this.LongDayNames = s.LongDayNames.slice(0);
      this.LongMonthNames = s.LongMonthNames.slice(0);
      this.LongTimeFormat = s.LongTimeFormat;
      this.NegCurrFormat = s.NegCurrFormat;
      this.ShortDateFormat = s.ShortDateFormat;
      this.ShortDayNames = s.ShortDayNames.slice(0);
      this.ShortMonthNames = s.ShortMonthNames.slice(0);
      this.ShortTimeFormat = s.ShortTimeFormat;
      this.ThousandSeparator = s.ThousandSeparator;
      this.TimeAMString = s.TimeAMString;
      this.TimePMString = s.TimePMString;
      this.TimeSeparator = s.TimeSeparator;
      this.TwoDigitYearCenturyWindow = s.TwoDigitYearCenturyWindow;
      return this;
    };
    this.GetJSLocale = function () {
      return Intl.DateTimeFormat().resolvedOptions().locale;
    };
    this.Create = function () {
      var Result = $lt1.$new();
      Result.$assign($lt1.Create$1($lt1.GetJSLocale()));
      return Result;
    };
    this.Create$1 = function (ALocale) {
      var Result = $lt1.$new();
      Result.LongDayNames = $impl.DefaultLongDayNames.slice(0);
      Result.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
      Result.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
      Result.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
      Result.DateTimeToStrFormat[0] = "c";
      Result.DateTimeToStrFormat[1] = "f";
      Result.DateSeparator = "-";
      Result.TimeSeparator = ":";
      Result.ShortDateFormat = "yyyy-mm-dd";
      Result.LongDateFormat = "ddd, yyyy-mm-dd";
      Result.ShortTimeFormat = "hh:nn";
      Result.LongTimeFormat = "hh:nn:ss";
      Result.DecimalSeparator = ".";
      Result.ThousandSeparator = ",";
      Result.TimeAMString = "AM";
      Result.TimePMString = "PM";
      Result.TwoDigitYearCenturyWindow = 50;
      Result.CurrencyFormat = 0;
      Result.NegCurrFormat = 0;
      Result.CurrencyDecimals = 2;
      Result.CurrencyString = "$";
      if ($lt1.InitLocaleHandler != null) $lt1.InitLocaleHandler($mod.UpperCase(ALocale),$lt1.$clone(Result));
      return Result;
    };
  },true);
  rtl.createClass(this,"Exception",$lt9,function () {
    $lt2 = this;
    this.LogMessageOnCreate = false;
    this.$init = function () {
      $lt9.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      if (this.LogMessageOnCreate) $lp("Created exception ",this.$classname," with message: ",Msg);
      return this;
    };
    this.CreateFmt = function (Msg, Args) {
      this.Create$1($mod.Format(Msg,Args));
      return this;
    };
  });
  rtl.createClass(this,"EAbort",$lt2,function () {
    $lt3 = this;
  });
  rtl.createClass(this,"EConvertError",$lt2,function () {
    $lt4 = this;
  });
  rtl.createClass(this,"EVariantError",$lt2,function () {
    $lt5 = this;
  });
  this.Trim = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'').replace(/[\s\uFEFF\xA0\x00-\x1f]+$/,'');
  };
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.AnsiSameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    Result = $mod.Format$1(Fmt,Args,$mod.FormatSettings);
    return Result;
  };
  this.Format$1 = function (Fmt, Args, aSettings) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9") && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === 255) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          var $tmp = Args[ArgN].VType;
          if ($tmp === 0) {
            Value = Args[ArgN].VJSValue}
           else if ($tmp === 19) {
            Value = Args[ArgN].VJSValue}
           else {
            $impl.DoFormatError(1,Fmt);
          };
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            $lp1($lp2(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = 255;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = $lp3(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === 255) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (Args[DoArg].VType !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + $lp2(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp = Fchar;
        if ($tmp === "D") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = $lp4("0",Index) + ToAdd}
           else $lp5($lp4("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp === "U") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToStr(Args[DoArg].VJSValue >>> 0)}
           else if (Checkarg(19,true)) ToAdd = $mod.IntToStr(Args[DoArg].VJSValue);
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = $lp4("0",Index) + ToAdd;
        } else if ($tmp === "E") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,2,3,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,2,3,Prec,aSettings);
        } else if ($tmp === "F") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,0,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,0,9999,Prec,aSettings);
        } else if ($tmp === "G") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,1,Prec,3,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,1,Prec,3,aSettings);
        } else if ($tmp === "N") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,3,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,3,9999,Prec,aSettings);
        } else if ($tmp === "M") {
          if (Checkarg(12,false)) {
            ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue / 10000,4,9999,Prec,aSettings)}
           else if (Checkarg(3,true)) ToAdd = $mod.FloatToStrF$1(Args[DoArg].VJSValue,4,9999,Prec,aSettings);
        } else if ($tmp === "S") {
          if (Checkarg(18,false)) {
            Hs = Args[DoArg].VJSValue}
           else if (Checkarg(9,true)) Hs = Args[DoArg].VJSValue;
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = $lp2(Hs,1,Index);
        } else if ($tmp === "P") {
          if (Checkarg(0,false)) {
            ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,8)}
           else if (Checkarg(0,true)) ToAdd = $mod.IntToHex(Args[DoArg].VJSValue,16);
        } else if ($tmp === "X") {
          if (Checkarg(0,false)) {
            vq = Args[DoArg].VJSValue;
            Index = 16;
          } else if (Checkarg(19,true)) {
            vq = Args[DoArg].VJSValue;
            Index = 31;
          };
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while ((rtl.shl(1,Index * 4) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = $lp4(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + $lp4(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  $lt6 = this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if (0 in Flags) REFlags = "g";
    if (1 in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    Result = $impl.IntTryStrToInt(S,res,$mod.FormatSettings.DecimalSeparator);
    return Result;
  };
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToIntDef$1 = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToInt = function (S) {
    var Result = 0;
    var R = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SErrInvalidInteger"),$lp6(18,S)]);
    Result = R;
    return Result;
  };
  this.StrToInt64 = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }})) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SErrInvalidInteger"),$lp6(18,S)]);
    Result = N;
    return Result;
  };
  this.TryStrToInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }});
    if (Result) res.set(R);
    return Result;
  };
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    Result = "";
    if (Value < 0) if (Value<0) Value = 0xFFFFFFFF + Value + 1;
    Result=Value.toString(16);
    Result = $mod.UpperCase(Result);
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  $lt7 = this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  var Rounds = "123456789:";
  this.FloatToDecimal = function (Value, Precision, Decimals) {
    var Result = $lt.$new();
    var Buffer = "";
    var InfNan = "";
    var OutPos = 0;
    var error = 0;
    var N = 0;
    var L = 0;
    var C = 0;
    var GotNonZeroBeforeDot = false;
    var BeforeDot = false;
    Result.Negative = false;
    Result.Exponent = 0;
    for (C = 0; C <= 19; C++) Result.Digits[C] = "0";
    if (Value === 0) return Result;
    Buffer = rtl.floatToStr(Value,24);
    N = 1;
    L = Buffer.length;
    while (Buffer.charAt(N - 1) === " ") N += 1;
    Result.Negative = Buffer.charAt(N - 1) === "-";
    if (Result.Negative) {
      N += 1}
     else if (Buffer.charAt(N - 1) === "+") N += 1;
    if (L >= (N + 2)) {
      InfNan = $lp2(Buffer,N,3);
      if (InfNan === "Inf") {
        Result.Digits[0] = "\x00";
        Result.Exponent = 32767;
        return Result;
      };
      if (InfNan === "Nan") {
        Result.Digits[0] = "\x00";
        Result.Exponent = -32768;
        return Result;
      };
    };
    OutPos = 0;
    Result.Exponent = 0;
    BeforeDot = true;
    GotNonZeroBeforeDot = false;
    while ((L >= N) && (Buffer.charAt(N - 1) !== "E")) {
      if (Buffer.charAt(N - 1) === ".") {
        BeforeDot = false}
       else {
        if (BeforeDot) {
          Result.Exponent += 1;
          Result.Digits[OutPos] = Buffer.charAt(N - 1);
          if (Buffer.charAt(N - 1) !== "0") GotNonZeroBeforeDot = true;
        } else Result.Digits[OutPos - 1] = Buffer.charAt(N - 1);
        OutPos += 1;
      };
      N += 1;
    };
    N += 1;
    if (N <= L) {
      $lp7($lp2(Buffer,N,(L - N) + 1),{get: function () {
          return C;
        }, set: function (v) {
          C = v;
        }},{get: function () {
          return error;
        }, set: function (v) {
          error = v;
        }});
      Result.Exponent += C;
    };
    N = OutPos;
    L = 19;
    while (N < L) {
      Result.Digits[N] = "0";
      N += 1;
    };
    if ((Decimals + Result.Exponent) < Precision) {
      N = Decimals + Result.Exponent}
     else N = Precision;
    if (N >= L) N = L - 1;
    if (N === 0) {
      if (Result.Digits[0] >= "5") {
        Result.Digits[0] = "1";
        Result.Digits[1] = "\x00";
        Result.Exponent += 1;
      } else Result.Digits[0] = "\x00";
    } else if (N > 0) {
      if (Result.Digits[N] >= "5") {
        do {
          Result.Digits[N] = "\x00";
          N -= 1;
          Result.Digits[N] = Rounds.charAt(($mod.StrToInt(Result.Digits[N]) + 1) - 1);
        } while (!((N === 0) || (Result.Digits[N] < ":")));
        if (Result.Digits[0] === ":") {
          Result.Digits[0] = "1";
          Result.Exponent += 1;
        };
      } else {
        Result.Digits[N] = "0";
        while ((N > -1) && (Result.Digits[N] === "0")) {
          Result.Digits[N] = "\x00";
          N -= 1;
        };
      };
    } else Result.Digits[0] = "\x00";
    if ((Result.Digits[0] === "\x00") && !GotNonZeroBeforeDot) {
      Result.Exponent = 0;
      Result.Negative = false;
    };
    return Result;
  };
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStr$1(Value,$mod.FormatSettings);
    return Result;
  };
  this.FloatToStr$1 = function (Value, aSettings) {
    var Result = "";
    Result = $mod.FloatToStrF$1(Value,1,15,0,aSettings);
    return Result;
  };
  this.FloatToStrF$1 = function (Value, format, Precision, Digits, aSettings) {
    var Result = "";
    var TS = "";
    var DS = "";
    DS = aSettings.DecimalSeparator;
    TS = aSettings.ThousandSeparator;
    var $tmp = format;
    if ($tmp === 1) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp === 2) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp === 0) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp === 3) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,TS)}
     else if ($tmp === 4) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,aSettings);
    if ((format !== 4) && (Result.length > 1) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS,TS);
    return Result;
  };
  this.TryStrToFloat$3 = function (S, res, aSettings) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if (aSettings.ThousandSeparator !== "") N = $mod.StringReplace(N,aSettings.ThousandSeparator,"",rtl.createSet(0));
    if (aSettings.DecimalSeparator !== ".") N = $mod.StringReplace(N,aSettings.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  this.StrToFloatDef = function (S, aDef) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat$3(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},$mod.FormatSettings)) Result = aDef;
    return Result;
  };
  this.StrToFloat = function (S) {
    var Result = 0.0;
    Result = $mod.StrToFloat$1(S,$mod.FormatSettings);
    return Result;
  };
  this.StrToFloat$1 = function (S, aSettings) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat$3(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},aSettings)) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SErrInvalidFloat"),$lp6(18,S)]);
    return Result;
  };
  this.FormatFloat = function (Fmt, aValue) {
    var Result = "";
    Result = $mod.FormatFloat$1(Fmt,aValue,$lt1.$clone($mod.FormatSettings));
    return Result;
  };
  var MaxPrecision = 18;
  this.FormatFloat$1 = function (Fmt, aValue, aSettings) {
    var Result = "";
    var E = 0.0;
    var FV = $lt.$new();
    var Section = "";
    var SectionLength = 0;
    var ThousandSep = false;
    var IsScientific = false;
    var DecimalPos = 0;
    var FirstDigit = 0;
    var LastDigit = 0;
    var RequestedDigits = 0;
    var ExpSize = 0;
    var Available = 0;
    var Current = 0;
    var PadZeroes = 0;
    var DistToDecimal = 0;
    function InitVars() {
      E = aValue;
      Section = "";
      SectionLength = 0;
      ThousandSep = false;
      IsScientific = false;
      DecimalPos = 0;
      FirstDigit = 2147483647;
      LastDigit = 0;
      RequestedDigits = 0;
      ExpSize = 0;
      Available = -1;
    };
    function ToResult(AChar) {
      Result = Result + AChar;
    };
    function AddToResult(AStr) {
      Result = Result + AStr;
    };
    function WriteDigit(ADigit) {
      if (ADigit === "\x00") return;
      DistToDecimal -= 1;
      if (DistToDecimal === -1) {
        AddToResult(aSettings.DecimalSeparator);
        ToResult(ADigit);
      } else {
        ToResult(ADigit);
        if (ThousandSep && ((DistToDecimal % 3) === 0) && (DistToDecimal > 1)) AddToResult(aSettings.ThousandSeparator);
      };
    };
    function GetDigit() {
      var Result = "";
      Result = "\x00";
      if (Current <= Available) {
        Result = FV.Digits[Current];
        Current += 1;
      } else if (DistToDecimal <= LastDigit) {
        DistToDecimal -= 1}
       else Result = "0";
      return Result;
    };
    function CopyDigit() {
      if (PadZeroes === 0) {
        WriteDigit(GetDigit())}
       else if (PadZeroes < 0) {
        PadZeroes += 1;
        if (DistToDecimal <= FirstDigit) {
          WriteDigit("0")}
         else DistToDecimal -= 1;
      } else {
        while (PadZeroes > 0) {
          WriteDigit(GetDigit());
          PadZeroes -= 1;
        };
        WriteDigit(GetDigit());
      };
    };
    function GetSections(SP) {
      var Result = 0;
      var FL = 0;
      var i = 0;
      var C = "";
      var Q = "";
      var inQuote = false;
      Result = 1;
      SP.get()[1] = -1;
      SP.get()[2] = -1;
      SP.get()[3] = -1;
      inQuote = false;
      Q = "\x00";
      i = 1;
      FL = Fmt.length;
      while (i <= FL) {
        C = Fmt.charAt(i - 1);
        var $tmp = C;
        if ($tmp === ";") {
          if (!inQuote) {
            if (Result > 3) throw $lt2.$create("Create$1",["Invalid float format"]);
            SP.get()[Result] = i;
            Result += 1;
          };
        } else if (($tmp === '"') || ($tmp === "'")) {
          if (inQuote) {
            inQuote = C !== Q}
           else {
            inQuote = true;
            Q = C;
          };
        };
        i += 1;
      };
      if (SP.get()[Result] === -1) SP.get()[Result] = FL + 1;
      return Result;
    };
    function AnalyzeFormat() {
      var I = 0;
      var Len = 0;
      var Q = "";
      var C = "";
      var InQuote = false;
      Len = Section.length;
      I = 1;
      InQuote = false;
      Q = "\x00";
      while (I <= Len) {
        C = Section.charAt(I - 1);
        if (C.charCodeAt() in rtl.createSet(34,39)) {
          if (InQuote) {
            InQuote = C !== Q}
           else {
            InQuote = true;
            Q = C;
          };
        } else if (!InQuote) {
          var $tmp = C;
          if ($tmp === ".") {
            if (DecimalPos === 0) DecimalPos = RequestedDigits + 1}
           else if ($tmp === ",") {
            ThousandSep = aSettings.ThousandSeparator !== "\x00"}
           else if (($tmp === "e") || ($tmp === "E")) {
            I += 1;
            if (I < Len) {
              C = Section.charAt(I - 1);
              IsScientific = C.charCodeAt() in rtl.createSet(45,43);
              if (IsScientific) while ((I < Len) && (Section.charAt((I + 1) - 1) === "0")) {
                ExpSize += 1;
                I += 1;
              };
              if (ExpSize > 4) ExpSize = 4;
            };
          } else if ($tmp === "#") {
            RequestedDigits += 1}
           else if ($tmp === "0") {
            if (RequestedDigits < FirstDigit) FirstDigit = RequestedDigits + 1;
            RequestedDigits += 1;
            LastDigit = RequestedDigits + 1;
          };
        };
        I += 1;
      };
      if (DecimalPos === 0) DecimalPos = RequestedDigits + 1;
      LastDigit = DecimalPos - LastDigit;
      if (LastDigit > 0) LastDigit = 0;
      FirstDigit = DecimalPos - FirstDigit;
      if (FirstDigit < 0) FirstDigit = 0;
    };
    function ValueOutSideScope() {
      var Result = false;
      Result = ((FV.Exponent >= 18) && !IsScientific) || (FV.Exponent === 0x7FF) || (FV.Exponent === 0x800);
      return Result;
    };
    function CalcRunVars() {
      var D = 0;
      var P = 0;
      if (IsScientific) {
        P = RequestedDigits;
        D = 9999;
      } else {
        P = 18;
        D = (RequestedDigits - DecimalPos) + 1;
      };
      FV.$assign($mod.FloatToDecimal(aValue,P,D));
      DistToDecimal = DecimalPos - 1;
      if (IsScientific) {
        PadZeroes = 0}
       else {
        PadZeroes = FV.Exponent - (DecimalPos - 1);
        if (PadZeroes >= 0) DistToDecimal = FV.Exponent;
      };
      Available = -1;
      while ((Available < 18) && (FV.Digits[Available + 1] !== "\x00")) Available += 1;
    };
    function FormatExponent(ASign, aExponent) {
      var Result = "";
      Result = $mod.IntToStr(aExponent);
      Result = $lp4("0",ExpSize - Result.length) + Result;
      if (aExponent < 0) {
        Result = "-" + Result}
       else if ((aExponent > 0) && (ASign === "+")) Result = ASign + Result;
      return Result;
    };
    var I = 0;
    var S = 0;
    var C = "";
    var Q = "";
    var PA = [];
    var InLiteral = false;
    PA = rtl.arraySetLength(PA,0,4);
    Result = "";
    InitVars();
    if (E > 0) {
      S = 1}
     else if (E < 0) {
      S = 2}
     else S = 3;
    PA[0] = 0;
    I = GetSections({get: function () {
        return PA;
      }, set: function (v) {
        PA = v;
      }});
    if ((I < S) || ((PA[S] - PA[S - 1]) === 0)) S = 1;
    SectionLength = PA[S] - PA[S - 1] - 1;
    Section = $lp2(Fmt,PA[S - 1] + 1,SectionLength);
    Section = rtl.strSetLength(Section,SectionLength);
    AnalyzeFormat();
    CalcRunVars();
    if ((SectionLength === 0) || ValueOutSideScope()) {
      Section=E.toPrecision(15);
      Result = Section;
    };
    I = 1;
    Current = 0;
    Q = " ";
    InLiteral = false;
    if (FV.Negative && (S === 1)) ToResult("-");
    while (I <= SectionLength) {
      C = Section.charAt(I - 1);
      if (C.charCodeAt() in rtl.createSet(34,39)) {
        if (InLiteral) {
          InLiteral = C !== Q}
         else {
          InLiteral = true;
          Q = C;
        };
      } else if (InLiteral) {
        ToResult(C)}
       else {
        var $tmp = C;
        if (($tmp === "0") || ($tmp === "#")) {
          CopyDigit()}
         else if (($tmp === ".") || ($tmp === ",")) {}
        else if (($tmp === "e") || ($tmp === "E")) {
          ToResult(C);
          I += 1;
          if (I <= Section.length) {
            C = Section.charAt(I - 1);
            if (C.charCodeAt() in rtl.createSet(43,45)) {
              AddToResult(FormatExponent(C,(FV.Exponent - DecimalPos) + 1));
              while ((I < SectionLength) && (Section.charAt((I + 1) - 1) === "0")) I += 1;
            };
          };
        } else {
          ToResult(C);
        };
      };
      I += 1;
    };
    return Result;
  };
  this.TrueBoolStrs = [];
  this.FalseBoolStrs = [];
  this.StrToBool = function (S) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SInvalidBoolean"),$lp6(18,S)]);
    return Result;
  };
  this.BoolToStr = function (B, UseBoolStrs) {
    var Result = "";
    if (UseBoolStrs) {
      $impl.CheckBoolStrs();
      if (B) {
        Result = $mod.TrueBoolStrs[0]}
       else Result = $mod.FalseBoolStrs[0];
    } else if (B) {
      Result = "-1"}
     else Result = "0";
    return Result;
  };
  this.StrToBoolDef = function (S, Default) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = Default;
    return Result;
  };
  this.TryStrToBool = function (S, Value) {
    var Result = false;
    var Temp = "";
    var I = 0;
    var D = 0.0;
    var Code = 0;
    Temp = $mod.UpperCase(S);
    $lp8(Temp,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }},{get: function () {
        return Code;
      }, set: function (v) {
        Code = v;
      }});
    Result = true;
    if (Code === 0) {
      Value.set(D !== 0.0)}
     else {
      $impl.CheckBoolStrs();
      for (var $l = 0, $end = rtl.length($mod.TrueBoolStrs) - 1; $l <= $end; $l++) {
        I = $l;
        if (Temp === $mod.UpperCase($mod.TrueBoolStrs[I])) {
          Value.set(true);
          return Result;
        };
      };
      for (var $l1 = 0, $end1 = rtl.length($mod.FalseBoolStrs) - 1; $l1 <= $end1; $l1++) {
        I = $l1;
        if (Temp === $mod.UpperCase($mod.FalseBoolStrs[I])) {
          Value.set(false);
          return Result;
        };
      };
      Result = false;
    };
    return Result;
  };
  this.OnGetEnvironmentVariable = null;
  this.OnGetEnvironmentString = null;
  this.OnGetEnvironmentVariableCount = null;
  this.Abort = function () {
    throw $lt3.$create("Create$1",[rtl.getResStr($mod,"SAbortError")]);
  };
  rtl.recNewT(this,"TTimeStamp",function () {
    $lt8 = this;
    this.Time = 0;
    this.Date = 0;
    this.$eq = function (b) {
      return (this.Time === b.Time) && (this.Date === b.Date);
    };
    this.$assign = function (s) {
      this.Time = s.Time;
      this.Date = s.Date;
      return this;
    };
  });
  this.TimeSeparator = "";
  this.DateSeparator = "";
  this.ShortDateFormat = "";
  this.LongDateFormat = "";
  this.ShortTimeFormat = "";
  this.LongTimeFormat = "";
  this.DecimalSeparator = "";
  this.ThousandSeparator = "";
  this.TimeAMString = "";
  this.TimePMString = "";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.DateDelta = 693594;
  this.MonthDays$a$clone = function (a) {
    var b = [];
    b.length = 2;
    for (var c = 0; c < 2; c++) b[c] = a[c].slice(0);
    return b;
  };
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = rtl.arraySetLength(null,"",12);
  this.LongMonthNames = rtl.arraySetLength(null,"",12);
  this.ShortDayNames = rtl.arraySetLength(null,"",7);
  this.LongDayNames = rtl.arraySetLength(null,"",7);
  this.FormatSettings = $lt1.$new();
  this.TwoDigitYearCenturyWindow = 50;
  this.JSDateToDateTime = function (aDate) {
    var Result = 0.0;
    Result = $mod.EncodeDate(aDate.getFullYear(),aDate.getMonth() + 1,aDate.getDate()) + $mod.EncodeTime(aDate.getHours(),aDate.getMinutes(),aDate.getSeconds(),aDate.getMilliseconds());
    return Result;
  };
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = $lt8.$new();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = $lp9(Math.abs($lp9(D)) % 86400000);
    Result.Date = 693594 + rtl.trunc($lp9(D) / 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (Year > 0) && (Year < 10000) && (Month >= 1) && (Month <= 12) && (Day > 0) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = rtl.trunc(Year / 100);
      ya = Year - (100 * c);
      date.set(((146097 * c) >>> 2) + ((1461 * ya) >>> 2) + rtl.trunc(((153 * Month) + 2) / 5) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (Hour < 24) && (Min < 60) && (Sec < 60) && (MSec < 1000);
    if (Result) Time.set(((Hour * 3600000) + (Min * 60000) + (Sec * 1000) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $lt4.$create("CreateFmt",["%s-%s-%s is not a valid date specification",$lp6(18,$mod.IntToStr(Year),18,$mod.IntToStr(Month),18,$mod.IntToStr(Day))]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $lt4.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",$lp6(18,$mod.IntToStr(Hour),18,$mod.IntToStr(Minute),18,$mod.IntToStr(Second),18,$mod.IntToStr(MilliSecond))]);
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = rtl.shl($lp9(date) + 693900,2) - 1;
      ly = rtl.trunc(j / 146097);
      j = j - (146097 * ly);
      ld = rtl.lw(j >>> 2);
      j = rtl.trunc((rtl.lw(ld << 2) + 3) / 1461);
      ld = rtl.lw(((rtl.lw(ld << 2) + 7) - (1461 * j)) >>> 2);
      lm = rtl.trunc(((5 * ld) - 3) / 153);
      ld = rtl.trunc((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(rtl.trunc(l / 3600000));
    l = l % 3600000;
    Minute.set(rtl.trunc(l / 60000));
    l = l % 60000;
    Second.set(rtl.trunc(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.ComposeDateTime = function (date, Time) {
    var Result = 0.0;
    if (date < 0) {
      Result = $lp9(date) - Math.abs($lp10(Time))}
     else Result = $lp9(date) + Math.abs($lp10(Time));
    return Result;
  };
  this.Date = function () {
    var Result = 0.0;
    Result = $lp9($mod.Now());
    return Result;
  };
  this.Time = function () {
    var Result = 0.0;
    Result = $mod.Now() - $mod.Date();
    return Result;
  };
  this.Now = function () {
    var Result = 0.0;
    Result = $mod.JSDateToDateTime(new Date());
    return Result;
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + (($lp9(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.DateToStr = function (date) {
    var Result = "";
    Result = $mod.DateToStr$1(date,$mod.FormatSettings);
    return Result;
  };
  this.DateToStr$1 = function (date, aSettings) {
    var Result = "";
    Result = $mod.FormatDateTime$1("ddddd",date,aSettings);
    return Result;
  };
  this.StrToDate = function (S) {
    var Result = 0.0;
    Result = $mod.StrToDate$3(S,$mod.FormatSettings);
    return Result;
  };
  this.StrToDate$2 = function (S, useformat, separator) {
    var Result = 0.0;
    var MSg = "";
    Result = $impl.IntStrToDate({get: function () {
        return MSg;
      }, set: function (v) {
        MSg = v;
      }},S,useformat,separator);
    if (MSg !== "") throw $lt4.$create("Create$1",[MSg]);
    return Result;
  };
  this.StrToDate$3 = function (S, aSettings) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,aSettings.ShortDateFormat,aSettings.DateSeparator);
    return Result;
  };
  this.TryStrToDate = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToDate$1(S,Value,$mod.FormatSettings);
    return Result;
  };
  this.TryStrToDate$1 = function (S, Value, aSettings) {
    var Result = false;
    Result = $mod.TryStrToDate$3(S,Value,aSettings.ShortDateFormat,aSettings.DateSeparator);
    return Result;
  };
  this.TryStrToDate$2 = function (S, Value, separator) {
    var Result = false;
    Result = $mod.TryStrToDate$3(S,Value,$mod.FormatSettings.ShortDateFormat,separator);
    return Result;
  };
  this.TryStrToDate$3 = function (S, Value, useformat, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToDate({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,useformat,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.StrToDateDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToDateDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToDateDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToDate$2(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.TimeToStr = function (Time) {
    var Result = "";
    Result = $mod.TimeToStr$1(Time,$mod.FormatSettings);
    return Result;
  };
  this.TimeToStr$1 = function (Time, aSettings) {
    var Result = "";
    Result = $mod.FormatDateTime$1("tt",Time,aSettings);
    return Result;
  };
  this.StrToTime = function (S) {
    var Result = 0.0;
    Result = $mod.StrToTime$2(S,$mod.FormatSettings);
    return Result;
  };
  this.StrToTime$2 = function (S, aSettings) {
    var Result = 0.0;
    var Msg = "";
    Result = $impl.IntStrToTime({get: function () {
        return Msg;
      }, set: function (v) {
        Msg = v;
      }},S,S.length,aSettings);
    if (Msg !== "") throw $lt4.$create("Create$1",[Msg]);
    return Result;
  };
  this.TryStrToTime = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToTime$1(S,Value,$lt1.$clone($mod.FormatSettings));
    return Result;
  };
  this.TryStrToTime$1 = function (S, Value, aSettings) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToTime({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,S.length,aSettings));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToTime$2 = function (S, Value, separator) {
    var Result = false;
    var Fmt = $lt1.$new();
    Fmt.$assign($lt1.Create());
    Fmt.TimeSeparator = separator;
    Result = $mod.TryStrToTime$1(S,Value,$lt1.$clone(Fmt));
    return Result;
  };
  this.StrToTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToTimeDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToTimeDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToTime$2(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.StrToDateTime = function (S) {
    var Result = 0.0;
    Result = $mod.StrToDateTime$1(S,$mod.FormatSettings);
    return Result;
  };
  this.StrToDateTime$1 = function (s, aSettings) {
    var Result = 0.0;
    var TimeStr = "";
    var DateStr = "";
    var PartsFound = 0;
    PartsFound = $impl.SplitDateTimeStr(s,{get: function () {
        return DateStr;
      }, set: function (v) {
        DateStr = v;
      }},{get: function () {
        return TimeStr;
      }, set: function (v) {
        TimeStr = v;
      }},aSettings);
    var $tmp = PartsFound;
    if ($tmp === 0) {
      Result = $mod.StrToDate("")}
     else if ($tmp === 1) {
      if (DateStr.length > 0) {
        Result = $mod.StrToDate$2(DateStr,aSettings.ShortDateFormat,aSettings.DateSeparator)}
       else Result = $mod.StrToTime(TimeStr)}
     else if ($tmp === 2) Result = $mod.ComposeDateTime($mod.StrToDate$2(DateStr,aSettings.ShortDateFormat,aSettings.DateSeparator),$mod.StrToTime(TimeStr));
    return Result;
  };
  this.TryStrToDateTime$1 = function (S, Value, aSettings) {
    var Result = false;
    var I = 0;
    var dtdate = 0.0;
    var dttime = 0.0;
    Result = false;
    I = $lp11(aSettings.TimeSeparator,S);
    if (I > 0) {
      while ((I > 0) && (S.charAt(I - 1) !== " ")) I -= 1;
      if (I > 0) {
        if (!$mod.TryStrToDate($lp2(S,1,I - 1),{get: function () {
            return dtdate;
          }, set: function (v) {
            dtdate = v;
          }})) return Result;
        if (!$mod.TryStrToTime($lp2(S,I + 1,S.length - I),{get: function () {
            return dttime;
          }, set: function (v) {
            dttime = v;
          }})) return Result;
        Value.set($mod.ComposeDateTime(dtdate,dttime));
        Result = true;
      } else Result = $mod.TryStrToTime(S,Value);
    } else Result = $mod.TryStrToDate(S,Value);
    return Result;
  };
  this.StrToDateTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToDateTimeDef$1(S,Defvalue,$lt1.$clone($mod.FormatSettings));
    return Result;
  };
  this.StrToDateTimeDef$1 = function (S, Defvalue, aSettings) {
    var Result = 0.0;
    if (!$mod.TryStrToDateTime$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},aSettings)) Result = Defvalue;
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    Result = $mod.FormatDateTime$1(FormatStr,DateTime,$mod.FormatSettings);
    return Result;
  };
  this.FormatDateTime$1 = function (FormatStr, DateTime, aSettings) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + $lp2(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp = Token;
        if (($tmp === "'") || ($tmp === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp === "A") || ($tmp === "a")) {
          if (($mod.CompareText($lp2(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText($lp2(FormatStr,P,4),"AMPM") === 0) || ($mod.CompareText($lp2(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp1 === "A") {
          if ($mod.CompareText($lp2(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString(aSettings.TimeAMString)}
             else StoreString(aSettings.TimePMString);
          } else if ($mod.CompareText($lp2(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText($lp2(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $lt4.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp1 === "\/") {
          StoreString(aSettings.DateSeparator);
        } else if ($tmp1 === ":") {
          StoreString(aSettings.TimeSeparator)}
         else if (($tmp1 === " ") || ($tmp1 === "C") || ($tmp1 === "D") || ($tmp1 === "H") || ($tmp1 === "M") || ($tmp1 === "N") || ($tmp1 === "S") || ($tmp1 === "T") || ($tmp1 === "Y") || ($tmp1 === "Z") || ($tmp1 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp2 = Token;
          if ($tmp2 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp2 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp2 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + ($lp9(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp3 = Count;
              if ($tmp3 === 1) {
                StoreInt(Month,0)}
               else if ($tmp3 === 2) {
                StoreInt(Month,2)}
               else if ($tmp3 === 3) {
                StoreString(aSettings.ShortMonthNames[Month - 1])}
               else {
                StoreString(aSettings.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp2 === "D") {
            var $tmp4 = Count;
            if ($tmp4 === 1) {
              StoreInt(Day,0)}
             else if ($tmp4 === 2) {
              StoreInt(Day,2)}
             else if ($tmp4 === 3) {
              StoreString(aSettings.ShortDayNames[DayOfWeek - 1])}
             else if ($tmp4 === 4) {
              StoreString(aSettings.LongDayNames[DayOfWeek - 1])}
             else if ($tmp4 === 5) {
              StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat(aSettings.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp2 === "H") {
            if (isInterval) {
              StoreInt(Hour + ($lp9(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp2 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + ($lp9(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp2 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + ($lp9(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp2 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp2 === "T") {
            if (Count === 1) {
              StoreFormat(aSettings.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp2 === "C") {
            StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false);
            if ((Hour !== 0) || (Minute !== 0) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp2 === "F") {
            StoreFormat(aSettings.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat(aSettings.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 0;
  this.CurrencyString = "";
  this.CurrToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF$1(Value / 10000,1,-1,0,$mod.FormatSettings);
    return Result;
  };
  this.StrToCurr = function (S) {
    var Result = 0;
    if (!$mod.TryStrToCurr(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SInvalidCurrency"),$lp6(18,S)]);
    return Result;
  };
  this.TryStrToCurr = function (S, Value) {
    var Result = false;
    var D = 0.0;
    Result = $mod.TryStrToFloat$3(S,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }},$mod.FormatSettings);
    if (Result) Value.set(rtl.trunc(D * 10000));
    return Result;
  };
  $mod.$implcode = function () {
    $impl.DefaultShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
    $impl.DefaultLongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
    $impl.DefaultShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
    $impl.DefaultLongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
    $impl.CheckBoolStrs = function () {
      if (rtl.length($mod.TrueBoolStrs) === 0) {
        $mod.TrueBoolStrs = rtl.arraySetLength($mod.TrueBoolStrs,"",1);
        $mod.TrueBoolStrs[0] = "True";
      };
      if (rtl.length($mod.FalseBoolStrs) === 0) {
        $mod.FalseBoolStrs = rtl.arraySetLength($mod.FalseBoolStrs,"",1);
        $mod.FalseBoolStrs[0] = "False";
      };
    };
    $impl.feInvalidFormat = 1;
    $impl.feMissingArgument = 2;
    $impl.feInvalidArgIndex = 3;
    $impl.DoFormatError = function (ErrCode, fmt) {
      var $tmp = ErrCode;
      if ($tmp === 1) {
        throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SInvalidFormat"),$lp6(18,fmt)])}
       else if ($tmp === 2) {
        throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SArgumentMissing"),$lp6(18,fmt)])}
       else if ($tmp === 3) throw $lt4.$create("CreateFmt",[rtl.getResStr($lm1,"SInvalidArgIndex"),$lp6(18,fmt)]);
    };
    $impl.maxdigits = 15;
    $impl.ReplaceDecimalSep = function (S, DS) {
      var Result = "";
      var P = 0;
      P = $lp11(".",S);
      if (P > 0) {
        Result = $lp2(S,1,P - 1) + DS + $lp2(S,P + 1,S.length - P)}
       else Result = S;
      return Result;
    };
    $impl.FormatGeneralFloat = function (Value, Precision, DS) {
      var Result = "";
      var P = 0;
      var PE = 0;
      var Q = 0;
      var Exponent = 0;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      Result = $mod.TrimLeft(Result);
      P = $lp11(".",Result);
      if (P === 0) return Result;
      PE = $lp11("E",Result);
      if (PE === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      Q = PE + 2;
      Exponent = 0;
      while (Q <= Result.length) {
        Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - 48;
        Q += 1;
      };
      if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
      if (((P + Exponent) < PE) && (Exponent > -6)) {
        Result = rtl.strSetLength(Result,PE - 1);
        if (Exponent >= 0) {
          for (var $l = 0, $end = Exponent - 1; $l <= $end; $l++) {
            Q = $l;
            Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
            P += 1;
          };
          Result = rtl.setCharAt(Result,P - 1,".");
          P = 1;
          if (Result.charAt(P - 1) === "-") P += 1;
          while ((Result.charAt(P - 1) === "0") && (P < Result.length) && ($lp2(Result,P + 1,DS.length) !== DS)) $lp12({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P,1);
        } else {
          $lp5($lp2("00000",1,-Exponent),{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 1);
          Result = rtl.setCharAt(Result,P - Exponent - 1,Result.charAt(P - Exponent - 1 - 1));
          Result = rtl.setCharAt(Result,P - 1,".");
          if (Exponent !== -1) Result = rtl.setCharAt(Result,P - Exponent - 1 - 1,"0");
        };
        Q = Result.length;
        while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
        if (Result.charAt(Q - 1) === ".") Q -= 1;
        if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
          Result = "0"}
         else Result = rtl.strSetLength(Result,Q);
      } else {
        while (Result.charAt(PE - 1 - 1) === "0") {
          $lp12({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt(PE - 1 - 1) === DS) {
          $lp12({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE - 1,1);
          PE -= 1;
        };
        if (Result.charAt((PE + 1) - 1) === "+") {
          $lp12({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},PE + 1,1)}
         else PE += 1;
        while (Result.charAt((PE + 1) - 1) === "0") $lp12({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1);
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
      var Result = "";
      var P = 0;
      DS = $mod.FormatSettings.DecimalSeparator;
      if ((Precision === -1) || (Precision > 15)) Precision = 15;
      Result = rtl.floatToStr(Value,Precision + 7);
      while (Result.charAt(0) === " ") $lp12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = $lp11("E",Result);
      if (P === 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS);
        return Result;
      };
      P += 2;
      if (Digits > 4) Digits = 4;
      Digits = (Result.length - P - Digits) + 1;
      if (Digits < 0) {
        $lp5($lp2("0000",1,-Digits),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P)}
       else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
        $lp12({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
        if (P > Result.length) {
          $lp12({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P - 2,2);
          break;
        };
        Digits -= 1;
      };
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatFixedFloat = function (Value, Digits, DS) {
      var Result = "";
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) $lp12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
      var Result = "";
      var P = 0;
      if (Digits === -1) {
        Digits = 2}
       else if (Digits > 15) Digits = 15;
      Result = rtl.floatToStr(Value,0,Digits);
      if ((Result !== "") && (Result.charAt(0) === " ")) $lp12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = $lp11(".",Result);
      if (P <= 0) P = Result.length + 1;
      Result = $impl.ReplaceDecimalSep(Result,DS);
      P -= 3;
      if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
        if (Result.charAt(P - 1 - 1) !== "-") $lp5(TS,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P);
        P -= 3;
      };
      return Result;
    };
    $impl.RemoveLeadingNegativeSign = function (AValue, DS, aThousandSeparator) {
      var Result = false;
      var i = 0;
      var TS = "";
      var StartPos = 0;
      Result = false;
      StartPos = 2;
      TS = aThousandSeparator;
      for (var $l = StartPos, $end = AValue.get().length; $l <= $end; $l++) {
        i = $l;
        Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get().charAt(i - 1) === TS);
        if (!Result) break;
      };
      if (Result && (AValue.get().charAt(0) === "-")) $lp12(AValue,1,1);
      return Result;
    };
    $impl.FormatNumberCurrency = function (Value, Digits, aSettings) {
      var Result = "";
      var Negative = false;
      var P = 0;
      var CS = "";
      var DS = "";
      var TS = "";
      DS = aSettings.DecimalSeparator;
      TS = aSettings.ThousandSeparator;
      CS = aSettings.CurrencyString;
      if (Digits === -1) {
        Digits = aSettings.CurrencyDecimals}
       else if (Digits > 18) Digits = 18;
      Result = rtl.floatToStr(Value / 10000,0,Digits);
      Negative = Result.charAt(0) === "-";
      if (Negative) $lp12({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},1,1);
      P = $lp11(".",Result);
      if (TS !== "") {
        if (P !== 0) {
          Result = $impl.ReplaceDecimalSep(Result,DS)}
         else P = Result.length + 1;
        P -= 3;
        while (P > 1) {
          $lp5(TS,{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},P);
          P -= 3;
        };
      };
      if (Negative) $impl.RemoveLeadingNegativeSign({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},DS,TS);
      if (!Negative) {
        var $tmp = aSettings.CurrencyFormat;
        if ($tmp === 0) {
          Result = CS + Result}
         else if ($tmp === 1) {
          Result = Result + CS}
         else if ($tmp === 2) {
          Result = CS + " " + Result}
         else if ($tmp === 3) Result = Result + " " + CS;
      } else {
        var $tmp1 = aSettings.NegCurrFormat;
        if ($tmp1 === 0) {
          Result = "(" + CS + Result + ")"}
         else if ($tmp1 === 1) {
          Result = "-" + CS + Result}
         else if ($tmp1 === 2) {
          Result = CS + "-" + Result}
         else if ($tmp1 === 3) {
          Result = CS + Result + "-"}
         else if ($tmp1 === 4) {
          Result = "(" + Result + CS + ")"}
         else if ($tmp1 === 5) {
          Result = "-" + Result + CS}
         else if ($tmp1 === 6) {
          Result = Result + "-" + CS}
         else if ($tmp1 === 7) {
          Result = Result + CS + "-"}
         else if ($tmp1 === 8) {
          Result = "-" + Result + " " + CS}
         else if ($tmp1 === 9) {
          Result = "-" + CS + " " + Result}
         else if ($tmp1 === 10) {
          Result = Result + " " + CS + "-"}
         else if ($tmp1 === 11) {
          Result = CS + " " + Result + "-"}
         else if ($tmp1 === 12) {
          Result = CS + " " + "-" + Result}
         else if ($tmp1 === 13) {
          Result = Result + "-" + " " + CS}
         else if ($tmp1 === 14) {
          Result = "(" + CS + " " + Result + ")"}
         else if ($tmp1 === 15) Result = "(" + Result + " " + CS + ")";
      };
      return Result;
    };
    $impl.RESpecials = "([\\$\\+\\[\\]\\(\\)\\\\\\.\\*\\^\\?\\|])";
    var WhiteSpace = " \b\t\n\f\r";
    var Digits = "0123456789";
    $impl.IntStrToDate = function (ErrorMsg, S, useformat, separator) {
      var Result = 0.0;
      function FixErrorMsg(errmarg) {
        ErrorMsg.set($mod.Format(rtl.getResStr($lm1,"SInvalidDateFormat"),$lp6(18,errmarg)));
      };
      var df = "";
      var d = 0;
      var m = 0;
      var y = 0;
      var ly = 0;
      var ld = 0;
      var lm = 0;
      var n = 0;
      var i = 0;
      var len = 0;
      var c = 0;
      var dp = 0;
      var mp = 0;
      var yp = 0;
      var which = 0;
      var s1 = "";
      var values = [];
      var YearMoreThenTwoDigits = false;
      values = rtl.arraySetLength(values,0,4);
      Result = 0;
      len = S.length;
      ErrorMsg.set("");
      while ((len > 0) && ($lp11(S.charAt(len - 1),WhiteSpace) > 0)) len -= 1;
      if (len === 0) {
        FixErrorMsg(S);
        return Result;
      };
      YearMoreThenTwoDigits = false;
      if (separator === "\x00") if ($mod.FormatSettings.DateSeparator !== "\x00") {
        separator = $mod.FormatSettings.DateSeparator}
       else separator = "-";
      df = $mod.UpperCase(useformat);
      yp = 0;
      mp = 0;
      dp = 0;
      which = 0;
      i = 0;
      while ((i < df.length) && (which < 3)) {
        i += 1;
        var $tmp = df.charAt(i - 1);
        if ($tmp === "Y") {
          if (yp === 0) {
            which += 1;
            yp = which;
          }}
         else if ($tmp === "M") {
          if (mp === 0) {
            which += 1;
            mp = which;
          }}
         else if ($tmp === "D") if (dp === 0) {
          which += 1;
          dp = which;
        };
      };
      for (i = 1; i <= 3; i++) values[i] = 0;
      s1 = "";
      n = 0;
      for (var $l = 1, $end = len; $l <= $end; $l++) {
        i = $l;
        if ($lp11(S.charAt(i - 1),Digits) > 0) s1 = s1 + S.charAt(i - 1);
        if ((separator !== " ") && (S.charAt(i - 1) === " ")) continue;
        if ((S.charAt(i - 1) === separator) || ((i === len) && ($lp11(S.charAt(i - 1),Digits) > 0))) {
          n += 1;
          if (n > 3) {
            FixErrorMsg(S);
            return Result;
          };
          if ((n === yp) && (s1.length > 2)) YearMoreThenTwoDigits = true;
          $lp7(s1,{a: n, p: values, get: function () {
              return this.p[this.a];
            }, set: function (v) {
              this.p[this.a] = v;
            }},{get: function () {
              return c;
            }, set: function (v) {
              c = v;
            }});
          if (c !== 0) {
            FixErrorMsg(S);
            return Result;
          };
          s1 = "";
        } else if ($lp11(S.charAt(i - 1),Digits) === 0) {
          FixErrorMsg(S);
          return Result;
        };
      };
      if ((which < 3) && (n > which)) {
        FixErrorMsg(S);
        return Result;
      };
      $mod.DecodeDate($mod.Date(),{get: function () {
          return ly;
        }, set: function (v) {
          ly = v;
        }},{get: function () {
          return lm;
        }, set: function (v) {
          lm = v;
        }},{get: function () {
          return ld;
        }, set: function (v) {
          ld = v;
        }});
      if (n === 3) {
        y = values[yp];
        m = values[mp];
        d = values[dp];
      } else {
        y = ly;
        if (n < 2) {
          d = values[1];
          m = lm;
        } else if (dp < mp) {
          d = values[1];
          m = values[2];
        } else {
          d = values[2];
          m = values[1];
        };
      };
      if ((y >= 0) && (y < 100) && !YearMoreThenTwoDigits) {
        ly = ly - $mod.TwoDigitYearCenturyWindow;
        y += rtl.trunc(ly / 100) * 100;
        if (($mod.TwoDigitYearCenturyWindow > 0) && (y < ly)) y += 100;
      };
      if (!$mod.TryEncodeDate(y,m,d,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) ErrorMsg.set(rtl.getResStr($lm1,"SErrInvalidDate"));
      return Result;
    };
    var AMPM_None = 0;
    var AMPM_AM = 1;
    var AMPM_PM = 2;
    var tiHour = 0;
    var tiMin = 1;
    var tiSec = 2;
    var tiMSec = 3;
    var Digits$1 = "0123456789";
    $impl.IntStrToTime = function (ErrorMsg, S, Len, aSettings) {
      var Result = 0.0;
      var AmPm = 0;
      var TimeValues = [];
      function SplitElements(TimeValues, AmPm) {
        var Result = false;
        var Cur = 0;
        var Offset = 0;
        var ElemLen = 0;
        var Err = 0;
        var TimeIndex = 0;
        var FirstSignificantDigit = 0;
        var Value = 0;
        var DigitPending = false;
        var MSecPending = false;
        var AmPmStr = "";
        var CurChar = "";
        var I = 0;
        var allowedchars = "";
        Result = false;
        AmPm.set(0);
        MSecPending = false;
        TimeIndex = 0;
        for (I = 0; I <= 3; I++) TimeValues.get()[I] = 0;
        Cur = 1;
        while ((Cur < Len) && (S.charAt(Cur - 1) === " ")) Cur += 1;
        Offset = Cur;
        if ((Cur > (Len - 1)) || (S.charAt(Cur - 1) === aSettings.TimeSeparator) || (S.charAt(Cur - 1) === aSettings.DecimalSeparator)) {
          return Result;
        };
        DigitPending = $lp11(S.charAt(Cur - 1),Digits$1) > 0;
        while (Cur <= Len) {
          CurChar = S.charAt(Cur - 1);
          if ($lp11(CurChar,Digits$1) > 0) {
            if (!DigitPending || (TimeIndex > 3)) {
              return Result;
            };
            Offset = Cur;
            if (CurChar !== "0") {
              FirstSignificantDigit = Offset}
             else FirstSignificantDigit = -1;
            while ((Cur < Len) && ($lp11(S.charAt((Cur + 1) - 1),Digits$1) > 0)) {
              if ((FirstSignificantDigit === -1) && (S.charAt(Cur - 1) !== "0")) FirstSignificantDigit = Cur;
              Cur += 1;
            };
            if (FirstSignificantDigit === -1) FirstSignificantDigit = Cur;
            ElemLen = (1 + Cur) - FirstSignificantDigit;
            if ((ElemLen <= 2) || ((ElemLen <= 3) && (TimeIndex === 3))) {
              $lp7($lp2(S,FirstSignificantDigit,ElemLen),{get: function () {
                  return Value;
                }, set: function (v) {
                  Value = v;
                }},{get: function () {
                  return Err;
                }, set: function (v) {
                  Err = v;
                }});
              TimeValues.get()[TimeIndex] = Value;
              TimeIndex += 1;
              DigitPending = false;
            } else {
              return Result;
            };
          } else if (CurChar === " ") {}
          else if (CurChar === aSettings.TimeSeparator) {
            if (DigitPending || (TimeIndex > 2)) {
              return Result;
            };
            DigitPending = true;
            MSecPending = false;
          } else if (CurChar === aSettings.DecimalSeparator) {
            if (DigitPending || MSecPending || (TimeIndex !== 3)) {
              return Result;
            };
            DigitPending = true;
            MSecPending = true;
          } else {
            if ((AmPm.get() !== 0) || DigitPending) {
              return Result;
            };
            Offset = Cur;
            allowedchars = aSettings.DecimalSeparator + " ";
            if (aSettings.TimeSeparator !== "\x00") allowedchars = allowedchars + aSettings.TimeSeparator;
            while ((Cur < Len) && ($lp11(S.charAt((Cur + 1) - 1),allowedchars) === 0) && ($lp11(S.charAt((Cur + 1) - 1),Digits$1) === 0)) Cur += 1;
            ElemLen = (1 + Cur) - Offset;
            AmPmStr = $lp2(S,Offset,ElemLen);
            if ($mod.CompareText(AmPmStr,aSettings.TimeAMString) === 0) {
              AmPm.set(1)}
             else if ($mod.CompareText(AmPmStr,aSettings.TimePMString) === 0) {
              AmPm.set(2)}
             else if ($mod.CompareText(AmPmStr,"AM") === 0) {
              AmPm.set(1)}
             else if ($mod.CompareText(AmPmStr,"PM") === 0) {
              AmPm.set(2)}
             else {
              return Result;
            };
            if (TimeIndex === 0) {
              DigitPending = true;
            } else {
              TimeIndex = 3 + 1;
              DigitPending = false;
            };
          };
          Cur += 1;
        };
        if ((TimeIndex === 0) || ((AmPm.get() !== 0) && ((TimeValues.get()[0] > 12) || (TimeValues.get()[0] === 0))) || DigitPending) return Result;
        Result = true;
        return Result;
      };
      TimeValues = rtl.arraySetLength(TimeValues,0,4);
      AmPm = 0;
      if (!SplitElements({get: function () {
          return TimeValues;
        }, set: function (v) {
          TimeValues = v;
        }},{get: function () {
          return AmPm;
        }, set: function (v) {
          AmPm = v;
        }})) {
        ErrorMsg.set($mod.Format(rtl.getResStr($lm1,"SErrInvalidTimeFormat"),$lp6(18,S)));
        return Result;
      };
      if ((AmPm === 2) && (TimeValues[0] !== 12)) {
        TimeValues[0] += 12}
       else if ((AmPm === 1) && (TimeValues[0] === 12)) TimeValues[0] = 0;
      if (!$mod.TryEncodeTime(TimeValues[0],TimeValues[1],TimeValues[2],TimeValues[3],{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) ErrorMsg.set($mod.Format(rtl.getResStr($lm1,"SErrInvalidTimeFormat"),$lp6(18,S)));
      return Result;
    };
    var WhiteSpace$1 = "\t\n\r ";
    $impl.SplitDateTimeStr = function (DateTimeStr, DateStr, TimeStr, aSettings) {
      var Result = 0;
      var p = 0;
      var DummyDT = 0.0;
      Result = 0;
      DateStr.set("");
      TimeStr.set("");
      DateTimeStr = $mod.Trim(DateTimeStr);
      if (DateTimeStr.length === 0) return Result;
      if ((aSettings.DateSeparator === " ") && (aSettings.TimeSeparator === " ") && ($lp11(" ",DateTimeStr) > 0)) {
        DateStr.set(DateTimeStr);
        return 1;
      };
      p = 1;
      if (aSettings.DateSeparator !== " ") {
        while ((p < DateTimeStr.length) && !($lp11(DateTimeStr.charAt((p + 1) - 1),WhiteSpace$1) > 0)) p += 1;
      } else {
        p = $lp11(aSettings.TimeSeparator,DateTimeStr);
        if (p !== 0) do {
          p -= 1;
        } while (!((p === 0) || ($lp11(DateTimeStr.charAt(p - 1),WhiteSpace$1) > 0)));
      };
      if (p === 0) p = DateTimeStr.length;
      DateStr.set($lp2(DateTimeStr,1,p));
      TimeStr.set($mod.Trim($lp2(DateTimeStr,p + 1,100)));
      if (TimeStr.get().length !== 0) {
        Result = 2}
       else {
        Result = 1;
        if (((aSettings.DateSeparator !== aSettings.TimeSeparator) && ($lp11(aSettings.TimeSeparator,DateStr.get()) > 0)) || ((aSettings.DateSeparator === aSettings.TimeSeparator) && !$mod.TryStrToDate(DateStr.get(),{get: function () {
            return DummyDT;
          }, set: function (v) {
            DummyDT = v;
          }}))) {
          TimeStr.set(DateStr.get());
          DateStr.set("");
        };
      };
      return Result;
    };
    $impl.IntTryStrToInt = function (S, res, aSep) {
      var Result = false;
      var Radix = 10;
      var N = "";
      var J = undefined;
      N = S;
      if (($lp11(aSep,N) !== 0) || ($lp11(".",N) !== 0)) return false;
      var $tmp = $lp2(N,1,1);
      if ($tmp === "$") {
        Radix = 16}
       else if ($tmp === "&") {
        Radix = 8}
       else if ($tmp === "%") Radix = 2;
      if ((Radix !== 16) && ($lp11("e",$mod.LowerCase(N)) !== 0)) return false;
      if (Radix !== 10) $lp12({get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},1,1);
      J = parseInt(N,Radix);
      Result = !isNaN(J);
      if (Result) res.set(rtl.trunc(J));
      return Result;
    };
    $mod.$resourcestrings = {SAbortError: {org: "Operation aborted"}};
  };
  $mod.$init = function () {
    $mod.ShortMonthNames = $impl.DefaultShortMonthNames.slice(0);
    $mod.LongMonthNames = $impl.DefaultLongMonthNames.slice(0);
    $mod.ShortDayNames = $impl.DefaultShortDayNames.slice(0);
    $mod.LongDayNames = $impl.DefaultLongDayNames.slice(0);
    $mod.FormatSettings.$assign($lt1.Create());
    $mod.TimeSeparator = $mod.FormatSettings.TimeSeparator;
    $mod.DateSeparator = $mod.FormatSettings.DateSeparator;
    $mod.ShortDateFormat = $mod.FormatSettings.ShortDateFormat;
    $mod.LongDateFormat = $mod.FormatSettings.LongDateFormat;
    $mod.ShortTimeFormat = $mod.FormatSettings.ShortTimeFormat;
    $mod.LongTimeFormat = $mod.FormatSettings.LongTimeFormat;
    $mod.DecimalSeparator = $mod.FormatSettings.DecimalSeparator;
    $mod.ThousandSeparator = $mod.FormatSettings.ThousandSeparator;
    $mod.TimeAMString = $mod.FormatSettings.TimeAMString;
    $mod.TimePMString = $mod.FormatSettings.TimePMString;
    $mod.CurrencyFormat = $mod.FormatSettings.CurrencyFormat;
    $mod.NegCurrFormat = $mod.FormatSettings.NegCurrFormat;
    $mod.CurrencyDecimals = $mod.FormatSettings.CurrencyDecimals;
    $mod.CurrencyString = $mod.FormatSettings.CurrencyString;
  };
},[]);
rtl.module("TypInfo",["System","SysUtils","Types","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  this.GetEnumName = function (TypeInfo, Value) {
    var Result = "";
    Result = TypeInfo.enumtype[Value];
    return Result;
  };
});
rtl.module("Classes",["System","RTLConsts","Types","SysUtils","JS","TypInfo"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lt15 = null;
  var $lt16 = null;
  var $lt17 = null;
  var $lt18 = null;
  var $lt19 = null;
  var $lt20 = null;
  var $lm = pas.System;
  var $lm1 = pas.SysUtils;
  var $lt21 = $lm1.Exception;
  var $lt22 = $lm.TObject;
  var $lm2 = pas.RTLConsts;
  var $lp = $lm.VarRecs;
  var $lp1 = $lm.Assigned;
  var $lp2 = $lm1.FreeAndNil;
  var $lt23 = $lm1.EConvertError;
  var $lp3 = $lm1.IntToStr;
  var $lp4 = $lm1.CompareText;
  var $lp5 = $lm.Pos;
  var $lp6 = $lm.Copy;
  var $lp7 = $lm1.CompareStr;
  var $lp8 = $lm1.IsValidIdent;
  this.$rtti.$MethodVar("TNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",$lm.$rtti["TObject"]]]), methodkind: 0});
  rtl.createClass(this,"EListError",$lt21,function () {
    $lt = this;
  });
  rtl.createClass(this,"EStringListError",$lt,function () {
    $lt1 = this;
  });
  rtl.createClass(this,"EComponentError",$lt21,function () {
    $lt2 = this;
  });
  $lt3 = this.TAlignment = {"0": "taLeftJustify", taLeftJustify: 0, "1": "taRightJustify", taRightJustify: 1, "2": "taCenter", taCenter: 2};
  this.$rtti.$Enum("TAlignment",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TAlignment});
  rtl.createClass(this,"TFPList",$lt22,function () {
    $lt4 = this;
    this.$init = function () {
      $lt22.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FCapacity = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      $lt22.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      Result = this.FList[Index];
      return Result;
    };
    this.Put = function (Index, Item) {
      if ((Index < 0) || (Index >= this.FCount)) this.RaiseIndexError(Index);
      this.FList[Index] = Item;
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < this.FCount) this.$class.Error(rtl.getResStr($lm2,"SListCapacityError"),"" + NewCapacity);
      if (NewCapacity === this.FCapacity) return;
      this.FList = rtl.arraySetLength(this.FList,undefined,NewCapacity);
      this.FCapacity = NewCapacity;
    };
    this.SetCount = function (NewCount) {
      if (NewCount < 0) this.$class.Error(rtl.getResStr($lm2,"SListCountError"),"" + NewCount);
      if (NewCount > this.FCount) {
        if (NewCount > this.FCapacity) this.SetCapacity(NewCount);
      };
      this.FCount = NewCount;
    };
    this.RaiseIndexError = function (Index) {
      this.$class.Error(rtl.getResStr($lm2,"SListIndexError"),"" + Index);
    };
    this.Destroy = function () {
      this.Clear();
      $lt22.Destroy.call(this);
    };
    this.Add = function (Item) {
      var Result = 0;
      if (this.FCount === this.FCapacity) this.Expand();
      this.FList[this.FCount] = Item;
      Result = this.FCount;
      this.FCount += 1;
      return Result;
    };
    this.Clear = function () {
      if (rtl.length(this.FList) > 0) {
        this.SetCount(0);
        this.SetCapacity(0);
      };
    };
    this.Delete = function (Index) {
      if ((Index < 0) || (Index >= this.FCount)) this.$class.Error(rtl.getResStr($lm2,"SListIndexError"),"" + Index);
      this.FCount = this.FCount - 1;
      this.FList.splice(Index,1);
      this.FCapacity -= 1;
    };
    this.Error = function (Msg, Data) {
      throw $lt.$create("CreateFmt",[Msg,$lp(18,Data)]);
    };
    this.Expand = function () {
      var Result = null;
      var IncSize = 0;
      if (this.FCount < this.FCapacity) return this;
      IncSize = 4;
      if (this.FCapacity > 3) IncSize = IncSize + 4;
      if (this.FCapacity > 8) IncSize = IncSize + 8;
      if (this.FCapacity > 127) IncSize += this.FCapacity >>> 2;
      this.SetCapacity(this.FCapacity + IncSize);
      Result = this;
      return Result;
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      var C = 0;
      Result = 0;
      C = this.FCount;
      while ((Result < C) && (this.FList[Result] != Item)) Result += 1;
      if (Result >= C) Result = -1;
      return Result;
    };
    this.IndexOfItem = function (Item, Direction) {
      var Result = 0;
      if (Direction === 0) {
        Result = this.IndexOf(Item)}
       else {
        Result = this.FCount - 1;
        while ((Result >= 0) && (this.FList[Result] != Item)) Result = Result - 1;
      };
      return Result;
    };
    this.Insert = function (Index, Item) {
      if ((Index < 0) || (Index > this.FCount)) this.$class.Error(rtl.getResStr($lm2,"SListIndexError"),"" + Index);
      this.FList.splice(Index,0,Item);
      this.FCapacity += 1;
      this.FCount += 1;
    };
    this.Last = function () {
      var Result = undefined;
      if (this.FCount === 0) {
        Result = null}
       else Result = this.Get(this.FCount - 1);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  $lt5 = this.TListNotification = {"0": "lnAdded", lnAdded: 0, "1": "lnExtracted", lnExtracted: 1, "2": "lnDeleted", lnDeleted: 2};
  rtl.createClass(this,"TList",$lt22,function () {
    $lt6 = this;
    this.$init = function () {
      $lt22.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      $lt22.$final.call(this);
    };
    this.Get = function (Index) {
      var Result = undefined;
      Result = this.FList.Get(Index);
      return Result;
    };
    this.Notify = function (aValue, Action) {
      if ($lp1(aValue)) ;
      if (Action === 1) ;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.Create$1 = function () {
      $lt22.Create.call(this);
      this.FList = $lt4.$create("Create");
      return this;
    };
    this.Destroy = function () {
      if (this.FList != null) this.Clear();
      $lp2({p: this, get: function () {
          return this.p.FList;
        }, set: function (v) {
          this.p.FList = v;
        }});
    };
    this.Add = function (Item) {
      var Result = 0;
      Result = this.FList.Add(Item);
      if ($lp1(Item)) this.Notify(Item,0);
      return Result;
    };
    this.Clear = function () {
      while (this.FList.FCount > 0) this.Delete(this.GetCount() - 1);
    };
    this.Delete = function (Index) {
      var V = undefined;
      V = this.FList.Get(Index);
      this.FList.Delete(Index);
      if ($lp1(V)) this.Notify(V,2);
    };
    this.IndexOf = function (Item) {
      var Result = 0;
      Result = this.FList.IndexOf(Item);
      return Result;
    };
    this.Remove = function (Item) {
      var Result = 0;
      Result = this.IndexOf(Item);
      if (Result !== -1) this.Delete(Result);
      return Result;
    };
  });
  rtl.createClass(this,"TPersistent",$lt22,function () {
    $lt7 = this;
    this.AssignError = function (Source) {
      var SourceName = "";
      if (Source !== null) {
        SourceName = Source.$classname}
       else SourceName = "Nil";
      throw $lt23.$create("Create$1",["Cannot assign a " + SourceName + " to a " + this.$classname + "."]);
    };
    this.AssignTo = function (Dest) {
      Dest.AssignError(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Assign = function (Source) {
      if (Source !== null) {
        Source.AssignTo(this)}
       else this.AssignError(null);
    };
  });
  rtl.createClass(this,"TStrings",$lt7,function () {
    $lt8 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FSpecialCharsInited = false;
      this.FAlwaysQuote = false;
      this.FQuoteChar = "";
      this.FDelimiter = "";
      this.FNameValueSeparator = "";
      this.FUpdateCount = 0;
      this.FLBS = 0;
      this.FLineBreak = "";
    };
    this.GetName = function (Index) {
      var Result = "";
      var V = "";
      this.GetNameValue(Index,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},{get: function () {
          return V;
        }, set: function (v) {
          V = v;
        }});
      return Result;
    };
    this.GetValue = function (Name) {
      var Result = "";
      var L = 0;
      var N = "";
      Result = "";
      L = this.IndexOfName(Name);
      if (L !== -1) this.GetNameValue(L,{get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.SetValue = function (Name, Value) {
      var L = 0;
      this.CheckSpecialChars();
      L = this.IndexOfName(Name);
      if (L === -1) {
        this.Add(Name + this.FNameValueSeparator + Value)}
       else this.Put(L,Name + this.FNameValueSeparator + Value);
    };
    this.Error = function (Msg, Data) {
      throw $lt1.$create("CreateFmt",[Msg,$lp(18,$lp3(Data))]);
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = this.GetCount();
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      if (Index === 0) ;
      Result = null;
      return Result;
    };
    this.Put = function (Index, S) {
      var Obj = null;
      Obj = this.GetObject(Index);
      this.Delete(Index);
      this.InsertObject(Index,S,Obj);
    };
    this.PutObject = function (Index, AObject) {
      if (Index === 0) return;
      if (AObject === null) return;
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) ;
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      Result = $lp4(s1,s2);
      return Result;
    };
    this.GetValueFromIndex = function (Index) {
      var Result = "";
      var N = "";
      this.GetNameValue(Index,{get: function () {
          return N;
        }, set: function (v) {
          N = v;
        }},{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }});
      return Result;
    };
    this.CheckSpecialChars = function () {
      if (!this.FSpecialCharsInited) {
        this.FQuoteChar = '"';
        this.FDelimiter = ",";
        this.FNameValueSeparator = "=";
        this.FLBS = $lm.DefaultTextLineBreakStyle;
        this.FSpecialCharsInited = true;
        this.FLineBreak = $lm.sLineBreak;
      };
    };
    this.Create$1 = function () {
      $lt22.Create.call(this);
      this.FAlwaysQuote = false;
      return this;
    };
    this.Destroy = function () {
      $lt22.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      Result = this.GetCount();
      this.Insert(this.GetCount(),S);
      return Result;
    };
    this.AddObject = function (S, AObject) {
      var Result = 0;
      Result = this.Add(S);
      this.PutObject(Result,AObject);
      return Result;
    };
    this.AddStrings = function (TheStrings) {
      var Runner = 0;
      for (var $l = 0, $end = TheStrings.GetCount() - 1; $l <= $end; $l++) {
        Runner = $l;
        this.AddObject(TheStrings.Get(Runner),TheStrings.GetObject(Runner));
      };
    };
    this.Assign = function (Source) {
      var S = null;
      if ($lt8.isPrototypeOf(Source)) {
        S = Source;
        this.BeginUpdate();
        try {
          this.Clear();
          this.FSpecialCharsInited = S.FSpecialCharsInited;
          this.FQuoteChar = S.FQuoteChar;
          this.FDelimiter = S.FDelimiter;
          this.FNameValueSeparator = S.FNameValueSeparator;
          this.FLBS = S.FLBS;
          this.FLineBreak = S.FLineBreak;
          this.AddStrings(S);
        } finally {
          this.EndUpdate();
        };
      } else $lt7.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      if (this.FUpdateCount === 0) this.SetUpdateState(true);
      this.FUpdateCount += 1;
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.SetUpdateState(false);
    };
    this.IndexOfName = function (Name) {
      var Result = 0;
      var len = 0;
      var S = "";
      this.CheckSpecialChars();
      Result = 0;
      while (Result < this.GetCount()) {
        S = this.Get(Result);
        len = $lp5(this.FNameValueSeparator,S) - 1;
        if ((len >= 0) && (this.DoCompareText(Name,$lp6(S,1,len)) === 0)) return Result;
        Result += 1;
      };
      Result = -1;
      return Result;
    };
    this.InsertObject = function (Index, S, AObject) {
      this.Insert(Index,S);
      this.PutObject(Index,AObject);
    };
    this.GetNameValue = function (Index, AName, AValue) {
      var L = 0;
      this.CheckSpecialChars();
      AValue.set(this.Get(Index));
      L = $lp5(this.FNameValueSeparator,AValue.get());
      if (L !== 0) {
        AName.set($lp6(AValue.get(),1,L - 1));
        AValue.set($lp6(AValue.get(),L + 1,AValue.get().length - L));
      } else AName.set("");
    };
  });
  rtl.recNewT(this,"TStringItem",function () {
    $lt9 = this;
    this.FString = "";
    this.FObject = null;
    this.$eq = function (b) {
      return (this.FString === b.FString) && (this.FObject === b.FObject);
    };
    this.$assign = function (s) {
      this.FString = s.FString;
      this.FObject = s.FObject;
      return this;
    };
  });
  $lt10 = this.TStringsSortStyle = {"0": "sslNone", sslNone: 0, "1": "sslUser", sslUser: 1, "2": "sslAuto", sslAuto: 2};
  rtl.createClass(this,"TStringList",$lt8,function () {
    $lt11 = this;
    this.$init = function () {
      $lt8.$init.call(this);
      this.FList = [];
      this.FCount = 0;
      this.FOnChange = null;
      this.FOnChanging = null;
      this.FDuplicates = 0;
      this.FCaseSensitive = false;
      this.FOwnsObjects = false;
      this.FSortStyle = 0;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FOnChange = undefined;
      this.FOnChanging = undefined;
      $lt8.$final.call(this);
    };
    this.GetSorted = function () {
      var Result = false;
      Result = this.FSortStyle in rtl.createSet(1,2);
      return Result;
    };
    this.Grow = function () {
      var NC = 0;
      NC = this.GetCapacity();
      if (NC >= 256) {
        NC = NC + rtl.trunc(NC / 4)}
       else if (NC === 0) {
        NC = 4}
       else NC = NC * 4;
      this.SetCapacity(NC);
    };
    this.InternalClear = function (FromIndex, ClearOnly) {
      var I = 0;
      if (FromIndex < this.FCount) {
        if (this.FOwnsObjects) {
          for (var $l = FromIndex, $end = this.FCount - 1; $l <= $end; $l++) {
            I = $l;
            this.FList[I].FString = "";
            $lp2({p: this.FList[I], get: function () {
                return this.p.FObject;
              }, set: function (v) {
                this.p.FObject = v;
              }});
          };
        } else {
          for (var $l1 = FromIndex, $end1 = this.FCount - 1; $l1 <= $end1; $l1++) {
            I = $l1;
            this.FList[I].FString = "";
          };
        };
        this.FCount = FromIndex;
      };
      if (!ClearOnly) this.SetCapacity(0);
    };
    this.CheckIndex = function (AIndex) {
      if ((AIndex < 0) || (AIndex >= this.FCount)) this.Error(rtl.getResStr($lm2,"SListIndexError"),AIndex);
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) {
        if (this.FOnChange != null) this.FOnChange(this);
      };
    };
    this.Changing = function () {
      if (this.FUpdateCount === 0) if (this.FOnChanging != null) this.FOnChanging(this);
    };
    this.Get = function (Index) {
      var Result = "";
      this.CheckIndex(Index);
      Result = this.FList[Index].FString;
      return Result;
    };
    this.GetCapacity = function () {
      var Result = 0;
      Result = rtl.length(this.FList);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FCount;
      return Result;
    };
    this.GetObject = function (Index) {
      var Result = null;
      this.CheckIndex(Index);
      Result = this.FList[Index].FObject;
      return Result;
    };
    this.Put = function (Index, S) {
      if (this.GetSorted()) this.Error(rtl.getResStr($lm2,"SSortedListError"),0);
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FString = S;
      this.Changed();
    };
    this.PutObject = function (Index, AObject) {
      this.CheckIndex(Index);
      this.Changing();
      this.FList[Index].FObject = AObject;
      this.Changed();
    };
    this.SetCapacity = function (NewCapacity) {
      if (NewCapacity < 0) this.Error(rtl.getResStr($lm2,"SListCapacityError"),NewCapacity);
      if (NewCapacity !== this.GetCapacity()) this.FList = rtl.arraySetLength(this.FList,$lt9,NewCapacity);
    };
    this.SetUpdateState = function (Updating) {
      if (Updating) {
        this.Changing()}
       else this.Changed();
    };
    this.InsertItem = function (Index, S) {
      this.InsertItem$1(Index,S,null);
    };
    this.InsertItem$1 = function (Index, S, O) {
      var It = $lt9.$new();
      this.Changing();
      if (this.FCount === this.GetCapacity()) this.Grow();
      It.FString = S;
      It.FObject = O;
      this.FList.splice(Index,0,It);
      this.FCount += 1;
      this.Changed();
    };
    this.DoCompareText = function (s1, s2) {
      var Result = 0;
      if (this.FCaseSensitive) {
        Result = $lp7(s1,s2)}
       else Result = $lp4(s1,s2);
      return Result;
    };
    this.Destroy = function () {
      this.InternalClear(0,false);
      $lt8.Destroy.call(this);
    };
    this.Add = function (S) {
      var Result = 0;
      if (!(this.FSortStyle === 2)) {
        Result = this.FCount}
       else if (this.Find(S,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) {
        var $tmp = this.FDuplicates;
        if ($tmp === 0) {
          return Result}
         else if ($tmp === 2) this.Error(rtl.getResStr($lm2,"SDuplicateString"),0);
      };
      this.InsertItem(Result,S);
      return Result;
    };
    this.Clear = function () {
      if (this.FCount === 0) return;
      this.Changing();
      this.InternalClear(0,false);
      this.Changed();
    };
    this.Delete = function (Index) {
      this.CheckIndex(Index);
      this.Changing();
      if (this.FOwnsObjects) $lp2({p: this.FList[Index], get: function () {
          return this.p.FObject;
        }, set: function (v) {
          this.p.FObject = v;
        }});
      this.FList.splice(Index,1);
      this.FList[this.GetCount() - 1].FString = "";
      this.FList[this.GetCount() - 1].FObject = null;
      this.FCount -= 1;
      this.Changed();
    };
    this.Find = function (S, Index) {
      var Result = false;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      Result = false;
      Index.set(-1);
      if (!this.GetSorted()) throw $lt.$create("Create$1",[rtl.getResStr($lm2,"SErrFindNeedsSortedList")]);
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + rtl.trunc((R - L) / 2);
        CompareRes = this.DoCompareText(S,this.FList[I].FString);
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            Result = true;
            if (this.FDuplicates !== 1) L = I;
          };
        };
      };
      Index.set(L);
      return Result;
    };
    this.Insert = function (Index, S) {
      if (this.FSortStyle === 2) {
        this.Error(rtl.getResStr($lm2,"SSortedListError"),0)}
       else {
        if ((Index < 0) || (Index > this.FCount)) this.Error(rtl.getResStr($lm2,"SListIndexError"),Index);
        this.InsertItem(Index,S);
      };
    };
  });
  rtl.createClass(this,"TCollectionItem",$lt7,function () {
    $lt12 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FCollection = null;
      this.FID = 0;
    };
    this.$final = function () {
      this.FCollection = undefined;
      $lt7.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FCollection != null) {
        Result = this.FCollection.FItems.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetCollection = function (Value) {
      if (Value !== this.FCollection) {
        if (this.FCollection != null) this.FCollection.RemoveItem(this);
        if (Value != null) Value.InsertItem(this);
      };
    };
    this.Changed = function (AllItems) {
      if ((this.FCollection !== null) && (this.FCollection.FUpdateCount === 0)) {
        if (AllItems) {
          this.FCollection.Update(null)}
         else this.FCollection.Update(this);
      };
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FCollection;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      this.Changed(false);
      if (Value === "") ;
    };
    this.Create$1 = function (ACollection) {
      $lt22.Create.call(this);
      this.SetCollection(ACollection);
      return this;
    };
    this.Destroy = function () {
      this.SetCollection(null);
      $lt22.Destroy.call(this);
    };
  });
  $lt13 = this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnExtracting", cnExtracting: 1, "2": "cnDeleting", cnDeleting: 2};
  rtl.createClass(this,"TCollection",$lt7,function () {
    $lt14 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FItemClass = null;
      this.FItems = null;
      this.FUpdateCount = 0;
      this.FNextID = 0;
    };
    this.$final = function () {
      this.FItemClass = undefined;
      this.FItems = undefined;
      $lt7.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FItems.FCount;
      return Result;
    };
    this.InsertItem = function (Item) {
      if (!this.FItemClass.isPrototypeOf(Item)) return;
      this.FItems.Add(Item);
      Item.FCollection = this;
      Item.FID = this.FNextID;
      this.FNextID += 1;
      this.SetItemName(Item);
      this.Notify(Item,0);
      this.Changed();
    };
    this.RemoveItem = function (Item) {
      var I = 0;
      this.Notify(Item,1);
      I = this.FItems.IndexOfItem(Item,1);
      if (I !== -1) this.FItems.Delete(I);
      Item.FCollection = null;
      this.Changed();
    };
    this.DoClear = function () {
      var Item = null;
      while (this.FItems.FCount > 0) {
        Item = rtl.getObject(this.FItems.Last());
        if (Item != null) Item.$destroy("Destroy");
      };
    };
    this.Changed = function () {
      if (this.FUpdateCount === 0) this.Update(null);
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FItems.Get(Index));
      return Result;
    };
    this.SetItemName = function (Item) {
      if (Item === null) ;
    };
    this.Update = function (Item) {
      if (Item === null) ;
    };
    this.Notify = function (Item, Action) {
      if (Item === null) ;
      if (Action === 0) ;
    };
    this.Create$1 = function (AItemClass) {
      $lt22.Create.call(this);
      this.FItemClass = AItemClass;
      this.FItems = $lt4.$create("Create");
      return this;
    };
    this.Destroy = function () {
      this.FUpdateCount = 1;
      try {
        this.DoClear();
      } finally {
        this.FUpdateCount = 0;
      };
      if (this.FItems != null) this.FItems.$destroy("Destroy");
      $lt22.Destroy.call(this);
    };
    this.Owner = function () {
      var Result = null;
      Result = this.GetOwner();
      return Result;
    };
    this.Add = function () {
      var Result = null;
      Result = this.FItemClass.$create("Create$1",[this]);
      return Result;
    };
    this.Assign = function (Source) {
      var I = 0;
      if ($lt14.isPrototypeOf(Source)) {
        this.Clear();
        for (var $l = 0, $end = Source.GetCount() - 1; $l <= $end; $l++) {
          I = $l;
          this.Add().Assign(Source.GetItem(I));
        };
        return;
      } else $lt7.Assign.call(this,Source);
    };
    this.BeginUpdate = function () {
      this.FUpdateCount += 1;
    };
    this.Clear = function () {
      if (this.FItems.FCount === 0) return;
      this.BeginUpdate();
      try {
        this.DoClear();
      } finally {
        this.EndUpdate();
      };
    };
    this.EndUpdate = function () {
      if (this.FUpdateCount > 0) this.FUpdateCount -= 1;
      if (this.FUpdateCount === 0) this.Changed();
    };
  });
  rtl.createClass(this,"TOwnedCollection",$lt14,function () {
    $lt15 = this;
    this.$init = function () {
      $lt14.$init.call(this);
      this.FOwner = null;
    };
    this.$final = function () {
      this.FOwner = undefined;
      $lt14.$final.call(this);
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Create$2 = function (AOwner, AItemClass) {
      this.FOwner = AOwner;
      $lt14.Create$1.call(this,AItemClass);
      return this;
    };
  });
  $lt16 = this.TOperation = {"0": "opInsert", opInsert: 0, "1": "opRemove", opRemove: 1};
  $lt17 = this.TComponentStateItem = {"0": "csLoading", csLoading: 0, "1": "csReading", csReading: 1, "2": "csWriting", csWriting: 2, "3": "csDestroying", csDestroying: 3, "4": "csDesigning", csDesigning: 4, "5": "csAncestor", csAncestor: 5, "6": "csUpdating", csUpdating: 6, "7": "csFixups", csFixups: 7, "8": "csFreeNotification", csFreeNotification: 8, "9": "csInline", csInline: 9, "10": "csDesignInstance", csDesignInstance: 10};
  $lt18 = this.TComponentStyleItem = {"0": "csInheritable", csInheritable: 0, "1": "csCheckPropAvail", csCheckPropAvail: 1, "2": "csSubComponent", csSubComponent: 2, "3": "csTransient", csTransient: 3};
  rtl.createClass(this,"TComponent",$lt7,function () {
    $lt19 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FOwner = null;
      this.FName = "";
      this.FTag = 0;
      this.FComponents = null;
      this.FFreeNotifies = null;
      this.FComponentState = {};
      this.FComponentStyle = {};
    };
    this.$final = function () {
      this.FOwner = undefined;
      this.FComponents = undefined;
      this.FFreeNotifies = undefined;
      this.FComponentState = undefined;
      this.FComponentStyle = undefined;
      $lt7.$final.call(this);
    };
    this.Insert = function (AComponent) {
      if (!(this.FComponents != null)) this.FComponents = $lt4.$create("Create");
      this.FComponents.Add(AComponent);
      AComponent.FOwner = this;
    };
    this.Remove = function (AComponent) {
      AComponent.FOwner = null;
      if (this.FComponents != null) {
        this.FComponents.Remove(AComponent);
        if (this.FComponents.FCount === 0) {
          this.FComponents.$destroy("Destroy");
          this.FComponents = null;
        };
      };
    };
    this.RemoveNotification = function (AComponent) {
      if (this.FFreeNotifies !== null) {
        this.FFreeNotifies.Remove(AComponent);
        if (this.FFreeNotifies.FCount === 0) {
          this.FFreeNotifies.$destroy("Destroy");
          this.FFreeNotifies = null;
          this.FComponentState = rtl.excludeSet(this.FComponentState,8);
        };
      };
    };
    this.SetReference = function (Enable) {
      var aField = null;
      var aValue = null;
      var aOwner = null;
      if (this.FName === "") return;
      if (this.FOwner != null) {
        aOwner = this.FOwner;
        aField = this.FOwner.$class.FieldAddress(this.FName);
        if (aField != null) {
          if (Enable) {
            aValue = this}
           else aValue = null;
          aOwner["" + aField["name"]] = aValue;
        };
      };
    };
    this.ChangeName = function (NewName) {
      this.FName = NewName;
    };
    this.GetOwner = function () {
      var Result = null;
      Result = this.FOwner;
      return Result;
    };
    this.Notification = function (AComponent, Operation) {
      var C = 0;
      if (Operation === 1) this.RemoveFreeNotification(AComponent);
      if (!(this.FComponents != null)) return;
      C = this.FComponents.FCount - 1;
      while (C >= 0) {
        rtl.getObject(this.FComponents.Get(C)).Notification(AComponent,Operation);
        C -= 1;
        if (C >= this.FComponents.FCount) C = this.FComponents.FCount - 1;
      };
    };
    this.SetDesigning = function (Value, SetChildren) {
      var Runner = 0;
      if (Value) {
        this.FComponentState = rtl.includeSet(this.FComponentState,4)}
       else this.FComponentState = rtl.excludeSet(this.FComponentState,4);
      if ((this.FComponents != null) && SetChildren) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).SetDesigning(Value,true);
      };
    };
    this.SetName = function (NewName) {
      if (this.FName === NewName) return;
      if ((NewName !== "") && !$lp8(NewName,false,false)) throw $lt2.$create("CreateFmt",[rtl.getResStr($lm2,"SInvalidName"),$lp(18,NewName)]);
      if (this.FOwner != null) {
        this.FOwner.ValidateRename(this,this.FName,NewName)}
       else this.ValidateRename(null,this.FName,NewName);
      this.SetReference(false);
      this.ChangeName(NewName);
      this.SetReference(true);
    };
    this.ValidateRename = function (AComponent, CurName, NewName) {
      if ((AComponent !== null) && ($lp4(CurName,NewName) !== 0) && (AComponent.FOwner === this) && (this.FindComponent(NewName) !== null)) throw $lt2.$create("CreateFmt",[rtl.getResStr($lm2,"SDuplicateName"),$lp(18,NewName)]);
      if ((4 in this.FComponentState) && (this.FOwner !== null)) this.FOwner.ValidateRename(AComponent,CurName,NewName);
    };
    this.ValidateContainer = function (AComponent) {
      AComponent.ValidateInsert(this);
    };
    this.ValidateInsert = function (AComponent) {
      if (AComponent === null) ;
    };
    this.Create$1 = function (AOwner) {
      this.FComponentStyle = rtl.createSet(0);
      if (AOwner != null) AOwner.InsertComponent(this);
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      var C = null;
      this.Destroying();
      if (this.FFreeNotifies != null) {
        I = this.FFreeNotifies.FCount - 1;
        while (I >= 0) {
          C = rtl.getObject(this.FFreeNotifies.Get(I));
          this.FFreeNotifies.Delete(I);
          C.Notification(this,1);
          if (this.FFreeNotifies === null) {
            I = 0}
           else if (I > this.FFreeNotifies.FCount) I = this.FFreeNotifies.FCount;
          I -= 1;
        };
        $lp2({p: this, get: function () {
            return this.p.FFreeNotifies;
          }, set: function (v) {
            this.p.FFreeNotifies = v;
          }});
      };
      this.DestroyComponents();
      if (this.FOwner !== null) this.FOwner.RemoveComponent(this);
      $lt22.Destroy.call(this);
    };
    this.BeforeDestruction = function () {
      if (!(3 in this.FComponentState)) this.Destroying();
    };
    this.DestroyComponents = function () {
      var acomponent = null;
      while (this.FComponents != null) {
        acomponent = rtl.getObject(this.FComponents.Last());
        this.Remove(acomponent);
        acomponent.$destroy("Destroy");
      };
    };
    this.Destroying = function () {
      var Runner = 0;
      if (3 in this.FComponentState) return;
      this.FComponentState = rtl.includeSet(this.FComponentState,3);
      if (this.FComponents != null) for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        Runner = $l;
        rtl.getObject(this.FComponents.Get(Runner)).Destroying();
      };
    };
    this.FindComponent = function (AName) {
      var Result = null;
      var I = 0;
      Result = null;
      if ((AName === "") || !(this.FComponents != null)) return Result;
      for (var $l = 0, $end = this.FComponents.FCount - 1; $l <= $end; $l++) {
        I = $l;
        if ($lp4(rtl.getObject(this.FComponents.Get(I)).FName,AName) === 0) {
          Result = rtl.getObject(this.FComponents.Get(I));
          return Result;
        };
      };
      return Result;
    };
    this.FreeNotification = function (AComponent) {
      if ((this.FOwner !== null) && (AComponent === this.FOwner)) return;
      if (!(this.FFreeNotifies != null)) this.FFreeNotifies = $lt4.$create("Create");
      if (this.FFreeNotifies.IndexOf(AComponent) === -1) {
        this.FFreeNotifies.Add(AComponent);
        AComponent.FreeNotification(this);
      };
    };
    this.RemoveFreeNotification = function (AComponent) {
      this.RemoveNotification(AComponent);
      AComponent.RemoveNotification(this);
    };
    this.InsertComponent = function (AComponent) {
      AComponent.ValidateContainer(this);
      this.ValidateRename(AComponent,"",AComponent.FName);
      if (AComponent.FOwner !== null) AComponent.FOwner.RemoveComponent(AComponent);
      this.Insert(AComponent);
      if (4 in this.FComponentState) AComponent.SetDesigning(true,true);
      this.Notification(AComponent,0);
    };
    this.RemoveComponent = function (AComponent) {
      this.Notification(AComponent,1);
      this.Remove(AComponent);
      AComponent.SetDesigning(false,true);
      this.ValidateRename(AComponent,AComponent.FName,"");
    };
    var $r = this.$rtti;
    $r.addProperty("Name",6,rtl.string,"FName","SetName");
    $r.addProperty("Tag",0,rtl.nativeint,"FTag","FTag",{Default: 0});
  });
  rtl.createClass(this,"TLoadHelper",$lt22,function () {
    $lt20 = this;
  });
  this.SetLoadHelperClass = function (aClass) {
    var Result = null;
    Result = $impl.GlobalLoadHelper;
    $impl.GlobalLoadHelper = aClass;
    return Result;
  };
  $mod.$implcode = function () {
    $impl.GlobalLoadHelper = null;
    $impl.ClassList = null;
  };
  $mod.$init = function () {
    $impl.ClassList = new Object();
  };
},[]);
rtl.module("Rtl.BrowserLoadHelper",["System","Classes","SysUtils","JS","Web"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lm = pas.Classes;
  var $lt1 = $lm.TLoadHelper;
  var $lp = $lm.SetLoadHelperClass;
  rtl.createClass(this,"TBrowserLoadHelper",$lt1,function () {
    $lt = this;
  });
  $mod.$init = function () {
    $lp($lt);
  };
});
rtl.module("browserconsole",["System","JS","Web","Rtl.BrowserLoadHelper","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lm = pas.System;
  var $lp = $lm.SetWriteCallBack;
  var $lm1 = pas.SysUtils;
  var $lp1 = $lm1.StringReplace;
  this.BrowserLineBreak = "\n";
  this.DefaultMaxConsoleLines = 25;
  this.DefaultConsoleStyle = ".pasconsole { " + this.BrowserLineBreak + "font-family: courier;" + this.BrowserLineBreak + "font-size: 14px;" + this.BrowserLineBreak + "background: #FFFFFF;" + this.BrowserLineBreak + "color: #000000;" + this.BrowserLineBreak + "display: block;" + this.BrowserLineBreak + "}";
  this.ConsoleElementID = "";
  this.ConsoleStyle = "";
  this.MaxConsoleLines = 0;
  this.ConsoleLinesToBrowserLog = false;
  this.ResetConsole = function () {
    if ($impl.LinesParent === null) return;
    while ($impl.LinesParent.firstElementChild !== null) $impl.LinesParent.removeChild($impl.LinesParent.firstElementChild);
    $impl.AppendLine();
  };
  this.InitConsole = function () {
    if ($impl.ConsoleElement === null) return;
    if ($impl.ConsoleElement.nodeName.toLowerCase() !== "body") {
      while ($impl.ConsoleElement.firstElementChild !== null) $impl.ConsoleElement.removeChild($impl.ConsoleElement.firstElementChild);
    };
    $impl.StyleElement = document.createElement("style");
    $impl.StyleElement.innerText = $mod.ConsoleStyle;
    $impl.ConsoleElement.appendChild($impl.StyleElement);
    $impl.LinesParent = document.createElement("div");
    $impl.ConsoleElement.appendChild($impl.LinesParent);
  };
  this.HookConsole = function () {
    $impl.ConsoleElement = null;
    if ($mod.ConsoleElementID !== "") $impl.ConsoleElement = document.getElementById($mod.ConsoleElementID);
    if ($impl.ConsoleElement === null) $impl.ConsoleElement = document.body;
    if ($impl.ConsoleElement === null) return;
    $mod.InitConsole();
    $mod.ResetConsole();
    $lp($impl.WriteConsole);
  };
  $mod.$implcode = function () {
    $impl.LastLine = null;
    $impl.StyleElement = null;
    $impl.LinesParent = null;
    $impl.ConsoleElement = null;
    $impl.AppendLine = function () {
      var CurrentCount = 0;
      var S = null;
      CurrentCount = 0;
      S = $impl.LinesParent.firstChild;
      while (S != null) {
        CurrentCount += 1;
        S = S.nextSibling;
      };
      while (CurrentCount > $mod.MaxConsoleLines) {
        CurrentCount -= 1;
        $impl.LinesParent.removeChild($impl.LinesParent.firstChild);
      };
      $impl.LastLine = document.createElement("div");
      $impl.LastLine.className = "pasconsole";
      $impl.LinesParent.appendChild($impl.LastLine);
    };
    $impl.EscapeString = function (S) {
      var Result = "";
      var CL = "";
      CL = $lp1(S,"<","&lt;",rtl.createSet(0));
      CL = $lp1(CL,">","&gt;",rtl.createSet(0));
      CL = $lp1(CL," ","&nbsp;",rtl.createSet(0));
      CL = $lp1(CL,"\r\n","<br>",rtl.createSet(0));
      CL = $lp1(CL,"\n","<br>",rtl.createSet(0));
      CL = $lp1(CL,"\r","<br>",rtl.createSet(0));
      Result = CL;
      return Result;
    };
    $impl.WriteConsole = function (S, NewLine) {
      var CL = "";
      CL = $impl.LastLine.innerHTML;
      CL = CL + $impl.EscapeString("" + S);
      $impl.LastLine.innerHTML = CL;
      if (NewLine) {
        if ($mod.ConsoleLinesToBrowserLog) window.console.log($impl.LastLine.innerText);
        $impl.AppendLine();
      };
    };
  };
  $mod.$init = function () {
    $mod.ConsoleLinesToBrowserLog = true;
    $mod.ConsoleElementID = "pasjsconsole";
    $mod.ConsoleStyle = $mod.DefaultConsoleStyle;
    $mod.MaxConsoleLines = 25;
    $mod.HookConsole();
  };
},[]);
rtl.module("CustApp",["System","Classes","SysUtils","Types","JS"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lm = pas.Classes;
  var $lt1 = $lm.TComponent;
  var $lm1 = pas.SysUtils;
  var $lt2 = $lm1.Exception;
  rtl.createClass(this,"TCustomApplication",$lt1,function () {
    $lt = this;
    this.$init = function () {
      $lt1.$init.call(this);
      this.FExceptObjectJS = undefined;
      this.FTerminated = false;
      this.FOptionChar = "";
      this.FCaseSensitiveOptions = false;
      this.FStopOnException = false;
      this.FExceptionExitCode = 0;
      this.FExceptObject = null;
    };
    this.$final = function () {
      this.FExceptObject = undefined;
      $lt1.$final.call(this);
    };
    this.Create$1 = function (AOwner) {
      $lt1.Create$1.call(this,AOwner);
      this.FOptionChar = "-";
      this.FCaseSensitiveOptions = true;
      this.FStopOnException = false;
      return this;
    };
    this.HandleException = function (Sender) {
      this.ShowException(this.FExceptObject);
      if (this.FStopOnException) this.Terminate$1(this.FExceptionExitCode);
      if (Sender === null) ;
    };
    this.Initialize = function () {
      this.FTerminated = false;
    };
    this.Run = function () {
      do {
        this.FExceptObject = null;
        this.FExceptObjectJS = null;
        try {
          this.DoRun();
        } catch ($e) {
          if ($lt2.isPrototypeOf($e)) {
            var E = $e;
            this.FExceptObject = E;
            this.FExceptObjectJS = E;
            this.HandleException(this);
          } else {
            this.FExceptObject = null;
            this.FExceptObjectJS = $e;
          }
        };
        break;
      } while (!this.FTerminated);
    };
    this.Terminate$1 = function (AExitCode) {
      this.FTerminated = true;
      rtl.exitcode = AExitCode;
    };
  });
});
rtl.module("browserapp",["System","Classes","SysUtils","Types","JS","Web","CustApp"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lm = pas.CustApp;
  var $lt1 = $lm.TCustomApplication;
  var $lm1 = pas.System;
  var $lp = $lm1.Writeln;
  var $lm2 = pas.SysUtils;
  var $lt2 = $lm2.Exception;
  var $lp1 = $lm2.FreeAndNil;
  var $lp2 = $lm1.Copy;
  var $lp3 = $lm2.LowerCase;
  rtl.createClass(this,"TBrowserApplication",$lt1,function () {
    $lt = this;
    this.GetHTMLElement = function (aID) {
      var Result = null;
      Result = document.getElementById(aID);
      if ((Result === null) && this.LogGetElementErrors()) $lp("Could not find element with ID ",aID);
      return Result;
    };
    this.DoRun = function () {
    };
    this.LogGetElementErrors = function () {
      var Result = false;
      Result = true;
      return Result;
    };
    this.ShowException = function (E) {
      var S = "";
      if (E !== null) {
        S = E.$classname + ": " + E.fMessage}
       else if (this.FExceptObjectJS) S = this.FExceptObjectJS.toString();
      window.alert("Unhandled exception caught:" + S);
    };
    this.HandleException = function (Sender) {
      if ($lt2.isPrototypeOf(this.FExceptObject)) this.ShowException(this.FExceptObject);
      $lt1.HandleException.call(this,Sender);
    };
  });
  this.ReloadEnvironmentStrings = function () {
    var I = 0;
    var S = "";
    var N = "";
    var A = [];
    var P = [];
    if ($impl.EnvNames != null) $lp1({p: $impl, get: function () {
        return this.p.EnvNames;
      }, set: function (v) {
        this.p.EnvNames = v;
      }});
    $impl.EnvNames = new Object();
    S = window.location.search;
    S = $lp2(S,2,S.length - 1);
    A = S.split("&");
    for (var $l = 0, $end = rtl.length(A) - 1; $l <= $end; $l++) {
      I = $l;
      P = A[I].split("=");
      N = $lp3(decodeURIComponent(P[0]));
      if (rtl.length(P) === 2) {
        $impl.EnvNames[N] = decodeURIComponent(P[1])}
       else if (rtl.length(P) === 1) $impl.EnvNames[N] = "";
    };
  };
  $mod.$implcode = function () {
    $impl.EnvNames = null;
    $impl.Params = [];
    $impl.ReloadParamStrings = function () {
      $impl.Params = rtl.arraySetLength($impl.Params,"",1);
      $impl.Params[0] = window.location.pathname;
    };
    $impl.GetParamCount = function () {
      var Result = 0;
      Result = rtl.length($impl.Params) - 1;
      return Result;
    };
    $impl.GetParamStr = function (Index) {
      var Result = "";
      Result = $impl.Params[Index];
      return Result;
    };
    $impl.MyGetEnvironmentVariable = function (EnvVar) {
      var Result = "";
      var aName = "";
      aName = $lp3(EnvVar);
      if ($impl.EnvNames.hasOwnProperty(aName)) {
        Result = "" + $impl.EnvNames[aName]}
       else Result = "";
      return Result;
    };
    $impl.MyGetEnvironmentVariableCount = function () {
      var Result = 0;
      Result = rtl.length(Object.getOwnPropertyNames($impl.EnvNames));
      return Result;
    };
    $impl.MyGetEnvironmentString = function (Index) {
      var Result = "";
      Result = "" + $impl.EnvNames[Object.getOwnPropertyNames($impl.EnvNames)[Index]];
      return Result;
    };
  };
  $mod.$init = function () {
    $lm1.IsConsole = true;
    $lm1.OnParamCount = $impl.GetParamCount;
    $lm1.OnParamStr = $impl.GetParamStr;
    $mod.ReloadEnvironmentStrings();
    $impl.ReloadParamStrings();
    $lm2.OnGetEnvironmentVariable = $impl.MyGetEnvironmentVariable;
    $lm2.OnGetEnvironmentVariableCount = $impl.MyGetEnvironmentVariableCount;
    $lm2.OnGetEnvironmentString = $impl.MyGetEnvironmentString;
  };
},["Rtl.BrowserLoadHelper"]);
rtl.module("strutils",["System","SysUtils","Types"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lm = pas.System;
  var $lp = $lm.upcase;
  var $lp1 = $lm.StringOfChar;
  this.Soundex = function (AText, ALength) {
    var Result = "";
    var S = "";
    var PS = "";
    var I = 0;
    var L = 0;
    Result = "";
    PS = "\x00";
    if (AText.length > 0) {
      Result = $lp(AText.charAt(0));
      I = 2;
      L = AText.length;
      while ((I <= L) && (Result.length < ALength)) {
        S = $impl.SScore.charAt(AText.charCodeAt(I - 1) - 1);
        if (!(S.charCodeAt() in rtl.createSet(48,105,PS.charCodeAt()))) Result = Result + S;
        if (S !== "i") PS = S;
        I += 1;
      };
    };
    L = Result.length;
    if (L < ALength) Result = Result + $lp1("0",ALength - L);
    return Result;
  };
  this.SoundexSimilar = function (AText, AOther, ALength) {
    var Result = false;
    Result = $mod.Soundex(AText,ALength) === $mod.Soundex(AOther,ALength);
    return Result;
  };
  this.SoundexSimilar$1 = function (AText, AOther) {
    var Result = false;
    Result = $mod.SoundexSimilar(AText,AOther,4);
    return Result;
  };
  this.SoundexProc = function (AText, AOther) {
    var Result = false;
    Result = $mod.SoundexSimilar$1(AText,AOther);
    return Result;
  };
  this.AnsiResemblesProc = null;
  this.ResemblesProc = null;
  this.RPosex$1 = function (Substr, Source, offs) {
    var Result = 0;
    Result = (new String(Source)).lastIndexOf(Substr,offs - 1) + 1;
    return Result;
  };
  this.RPos = function (c, S) {
    var Result = 0;
    Result = $mod.RPosex$1(c,S,S.length);
    return Result;
  };
  $mod.$implcode = function () {
    $impl.SScore = "00000000000000000000000000000000" + "00000000000000000000000000000000" + "0123012i02245501262301i2i2" + "000000" + "0123012i02245501262301i2i2" + "00000000000000000000000000000000" + "00000000000000000000000000000000" + "00000000000000000000000000000000" + "00000000000000000000000000000000" + "00000";
  };
  $mod.$init = function () {
    $mod.AnsiResemblesProc = $mod.SoundexProc;
    $mod.ResemblesProc = $mod.SoundexProc;
  };
},["JS"]);
rtl.module("webrouter",["System","Classes","SysUtils","Web"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lm = pas.SysUtils;
  var $lt15 = $lm.Exception;
  var $lm1 = pas.Classes;
  var $lm2 = pas.System;
  var $lt16 = $lm2.TObject;
  var $lp = $lm2.Copy;
  var $lp1 = $lm2.Pos;
  var $lt17 = $lm1.TStringList;
  var $lt18 = $lm1.TCollectionItem;
  var $lp2 = $lm2.Delete;
  var $lp3 = $lm.CompareText;
  var $lm3 = null;
  var $lp4 = null;
  var $lt19 = $lm1.TCollection;
  var $lt20 = $lm1.TComponent;
  var $lp5 = $lm2.VarRecs;
  var $lp6 = $lm.FreeAndNil;
  var $lm4 = null;
  var $lp7 = null;
  var $lp8 = $lm.IntToStr;
  var $lp9 = null;
  rtl.createClass(this,"EHTTPRoute",$lt15,function () {
    $lt = this;
  });
  rtl.recNewT(this,"TScrollPoint",function () {
    $lt1 = this;
    this.X = 0.0;
    this.Y = 0.0;
    this.$eq = function (b) {
      return (this.X === b.X) && (this.Y === b.Y);
    };
    this.$assign = function (s) {
      this.X = s.X;
      this.Y = s.Y;
      return this;
    };
  });
  this.$rtti.$Class("TRoute");
  this.$rtti.$RefToProcVar("TRouteEvent",{procsig: rtl.newTIProcSig([["URl",rtl.string],["aRoute",this.$rtti["TRoute"]],["Params",$lm1.$rtti["TStrings"]]])});
  $lt2 = this.TTransitionResult = {"0": "trOK", trOK: 0, "1": "trError", trError: 1, "2": "trAbort", trAbort: 2};
  $lt3 = this.THistoryKind = {"0": "hkAuto", hkAuto: 0, "1": "hkAbstract", hkAbstract: 1, "2": "hkHash", hkHash: 2, "3": "hkHTML5", hkHTML5: 3};
  rtl.createClass(this,"THistory",$lt16,function () {
    $lt4 = this;
    this.$init = function () {
      $lt16.$init.call(this);
      this.FOnAllow = null;
      this.FRouter = null;
      this.FOnChange = null;
      this.FOnReady = null;
      this.FOnError = null;
      this.FCurrent = null;
      this.FBase = "";
    };
    this.$final = function () {
      this.FOnAllow = undefined;
      this.FRouter = undefined;
      this.FOnChange = undefined;
      this.FOnReady = undefined;
      this.FOnError = undefined;
      this.FCurrent = undefined;
      $lt16.$final.call(this);
    };
    this.GetCurrent = function () {
      var Result = null;
      Result = this.FCurrent;
      return Result;
    };
    this.SetupListeners = function () {
    };
    this.Create$2 = function (aRouter, aBase) {
      this.FRouter = aRouter;
      this.FBase = aBase;
      return this;
    };
    this.NormalizeHash = function (aHash) {
      var Result = "";
      Result = aHash;
      if ($lp(Result,1,1) !== "\/") Result = "\/" + Result;
      return Result;
    };
    this.UpdateRoute = function (aRoute) {
      this.FCurrent = aRoute;
      if (this.FOnChange != null) this.FOnChange(aRoute);
    };
    this.Destroy = function () {
      $lt16.Destroy.call(this);
    };
    this.ExpectScroll = function () {
      var Result = false;
      Result = (this.FRouter != null) && (this.FRouter.FOnScroll != null);
      return Result;
    };
    this.SupportsScroll = function () {
      var Result = false;
      Result = $lt13.supportsPushState() && this.ExpectScroll();
      return Result;
    };
    this.getLocation = function (base) {
      var Result = "";
      var path = "";
      path = window.location.pathname;
      if ((base !== "") && ($lp1(base,path) === 1)) path = $lp(path,base.length + 1,path.length - base.length);
      Result = path;
      if (Result === "") Result = "\/";
      Result = Result + window.location.search + window.location.hash;
      return Result;
    };
    this.Push = function (location) {
      var Result = 0;
      var Old = null;
      Old = this.GetCurrent();
      Result = this.TransitionTo(location);
      if (Result === 0) {
        Result = this.doPush(location);
        if (Result === 0) $lt12.handle(this.FRouter,this.GetCurrent(),Old,false);
      };
      return Result;
    };
    this.TransitionTo = function (aLocation) {
      var Result = 0;
      var Params = null;
      var R = null;
      Params = $lt17.$create("Create$1");
      try {
        R = this.FRouter.FindHTTPRoute(aLocation,Params);
        var $tmp = this.ConfirmTransition(R,Params);
        if ($tmp === 0) {
          R = this.FRouter.DoRouteRequest(R,aLocation,Params);
          this.UpdateRoute(R);
          if (this.FOnReady != null) this.FOnReady(this,aLocation,R);
        } else if ($tmp === 1) if (this.FOnError != null) this.FOnError(this,aLocation,R);
      } finally {
        Params = rtl.freeLoc(Params);
      };
      Result = 0;
      return Result;
    };
    this.ConfirmTransition = function (aRoute, Params) {
      var Result = 0;
      var Old = null;
      var allow = false;
      Old = this.GetCurrent();
      allow = true;
      if (this.FOnAllow != null) this.FOnAllow(this,Old,aRoute,Params,{get: function () {
          return allow;
        }, set: function (v) {
          allow = v;
        }});
      if (!allow) {
        this.ensureURL(false);
        Result = 2;
      };
      Result = 0;
      return Result;
    };
  });
  rtl.createClass(this,"TAbstractHistory",$lt4,function () {
    $lt5 = this;
    this.$init = function () {
      $lt4.$init.call(this);
      this.FIndex = 0;
      this.FStack = [];
    };
    this.$final = function () {
      this.FStack = undefined;
      $lt4.$final.call(this);
    };
    this.MaybeGrow = function (AIndex) {
      if ((AIndex + 1) > rtl.length(this.FStack)) this.FStack = rtl.arraySetLength(this.FStack,"",AIndex + 1);
    };
    this.doPush = function (location) {
      var Result = 0;
      this.FIndex += 1;
      this.MaybeGrow(this.FIndex);
      this.FStack[this.FIndex] = location;
      Result = 0;
      return Result;
    };
    this.Create$2 = function (router, base) {
      $lt4.Create$2.apply(this,arguments);
      this.FStack = rtl.arraySetLength(this.FStack,"",0);
      this.FIndex = -1;
      return this;
    };
    this.GetCurrentLocation = function () {
      var Result = "";
      var I = 0;
      var Route = "";
      I = rtl.length(this.FStack) - 1;
      if (I >= 0) {
        Route = this.FStack[I]}
       else Result = "\/";
      Result = Route;
      return Result;
    };
    this.ensureURL = function (Push) {
      if (Push) ;
    };
  });
  rtl.createClass(this,"THashHistory",$lt4,function () {
    $lt6 = this;
    this.$init = function () {
      $lt4.$init.call(this);
      this.FlastHash = "";
    };
    this.DoHashChange = function () {
      var NewHash = "";
      var Old = null;
      NewHash = this.$class.NormalizeHash(this.$class.getHash());
      if (NewHash === this.FlastHash) return;
      Old = this.GetCurrent();
      if (this.TransitionTo(NewHash) === 0) {
        $lt12.handle(this.FRouter,this.GetCurrent(),Old,true);
        this.FlastHash = NewHash;
      } else this.$class.replaceHash(this.FlastHash);
    };
    this.SetupListeners = function () {
      if (this.SupportsScroll()) $lt12.Setup();
      if ($lt13.supportsPushState()) {
        window.addEventListener("popstate",rtl.createCallback(this,"DoHashChange"))}
       else window.addEventListener("hashchange",rtl.createCallback(this,"DoHashChange"));
    };
    this.doPush = function (location) {
      var Result = 0;
      var L = "";
      L = this.$class.NormalizeHash(location);
      this.FlastHash = L;
      this.$class.pushHash(L);
      Result = 0;
      return Result;
    };
    this.ensureURL = function (push) {
      var aHash = "";
      var CURL = "";
      CURL = this.$class.NormalizeHash(this.FlastHash);
      aHash = this.$class.getHash();
      if (aHash !== CURL) if (push) {
        this.$class.pushHash(CURL)}
       else this.$class.replaceHash(CURL);
    };
    this.GetCurrentLocation = function () {
      var Result = "";
      Result = this.$class.getHash();
      return Result;
    };
    this.pushHash = function (path) {
      if ($lt13.supportsPushState()) {
        $lt13.PushState(this.getUrl(path),false)}
       else window.location.hash = path;
    };
    this.replaceHash = function (path) {
      var H = "";
      H = this.getHash();
      if (H === path) return;
      if ($lt13.supportsPushState()) {
        $lt13.ReplaceState(this.getUrl(path))}
       else window.location.replace(this.getUrl(path));
    };
    this.getUrl = function (APath) {
      var Result = "";
      var HRef = "";
      var Idx = 0;
      HRef = window.location.href;
      Idx = $lp1("#",HRef);
      if (Idx === 0) {
        Result = HRef}
       else Result = $lp(HRef,1,Idx - 1);
      Result = Result + "#" + APath;
      return Result;
    };
    this.getHash = function () {
      var Result = "";
      var HRef = "";
      var Idx = 0;
      HRef = window.location.href;
      Idx = $lp1("#",HRef);
      if (Idx === 0) {
        Result = ""}
       else Result = $lp(HRef,Idx + 1,HRef.length - Idx);
      return Result;
    };
  });
  rtl.createClass(this,"THTMLHistory",$lt4,function () {
    $lt7 = this;
    this.$init = function () {
      $lt4.$init.call(this);
      this.FlastLocation = "";
    };
    this.DoStateChange = function () {
      var NewLocation = "";
      var Old = null;
      NewLocation = this.$class.getLocation(this.FBase);
      if (NewLocation === this.FlastLocation) return;
      Old = this.GetCurrent();
      if (this.TransitionTo(NewLocation) === 0) {
        $lt12.handle(this.FRouter,this.GetCurrent(),Old,true);
        this.FlastLocation = NewLocation;
      } else this.$class.replaceState(this.FlastLocation);
    };
    this.SetupListeners = function () {
      window.addEventListener("popstate",rtl.createCallback(this,"DoStateChange"));
    };
    this.doPush = function (location) {
      var Result = 0;
      this.$class.pushState(this.getUrl(location),false);
      Result = 0;
      return Result;
    };
    this.ensureURL = function (push) {
      var URL = "";
      var Actual = "";
      var Expected = "";
      Actual = this.GetCurrentLocation();
      Expected = this.FlastLocation;
      if (Actual !== Expected) {
        URL = this.getUrl(Expected);
        if (push) {
          this.$class.pushState(URL,false)}
         else this.$class.replaceState(URL);
      };
    };
    this.GetCurrentLocation = function () {
      var Result = "";
      Result = window.location;
      return Result;
    };
    this.pushState = function (path, doReplace) {
      $lt13.PushState(path,doReplace);
    };
    this.replaceState = function (path) {
      this.pushState(path,true);
    };
    this.getUrl = function (ALocation) {
      var Result = "";
      Result = $mod.IncludeHTTPPathDelimiter(this.FBase);
      while ((ALocation !== "") && ($lp(ALocation,1,1) === "\/")) ALocation = $lp(ALocation,2,ALocation.length - 1);
      Result = this.FBase + ALocation;
      return Result;
    };
  });
  rtl.createClass(this,"TRoute",$lt18,function () {
    $lt8 = this;
    this.$init = function () {
      $lt18.$init.call(this);
      this.FDefault = false;
      this.FEvent = null;
      this.FURLPattern = "";
    };
    this.$final = function () {
      this.FEvent = undefined;
      $lt18.$final.call(this);
    };
    this.SetURLPattern = function (AValue) {
      var V = "";
      V = this.$class.NormalizeURLPattern(AValue);
      if (this.FURLPattern === V) return;
      this.FURLPattern = V;
    };
    this.NormalizeURLPattern = function (AValue) {
      var Result = "";
      var V = "";
      V = $mod.IncludeHTTPPathDelimiter(AValue);
      if ((V !== "\/") && (V.charAt(0) === "\/")) $lp2({get: function () {
          return V;
        }, set: function (v) {
          V = v;
        }},1,1);
      Result = V;
      return Result;
    };
    this.Matches = function (APattern) {
      var Result = false;
      Result = $lp3(this.FURLPattern,this.$class.NormalizeURLPattern(APattern)) === 0;
      return Result;
    };
    this.MatchPattern = function (Path, L) {
      var $Self = this;
      var Result = false;
      function StartsWith(C, S) {
        var Result = false;
        Result = (S.length > 0) && (S.charAt(0) === C);
        return Result;
      };
      function EndsWith(C, S) {
        var Result = false;
        var L = 0;
        L = S.length;
        Result = (L > 0) && (S.charAt(L - 1) === C);
        return Result;
      };
      function ExtractNextPathLevel(ALeft, ALvl, ARight, ADelim) {
        var P = 0;
        if (ALvl.get() !== ADelim) {
          ALeft.set(ALeft.get() + ALvl.get());
          if (StartsWith(ADelim,ARight.get())) {
            ALeft.set(ALeft.get() + ADelim);
            $lp2(ARight,1,1);
          };
        };
        P = $lp1(ADelim,ARight.get());
        if (P === 0) P = ARight.get().length + 1;
        ALvl.set($lp(ARight.get(),1,P - 1));
        ARight.set($lp(ARight.get(),P,2147483647));
      };
      function ExtractPrevPathLevel(ALeft, ALvl, ARight, ADelim) {
        var P = 0;
        var L = 0;
        if (ALvl.get() !== ADelim) {
          ARight.set(ALvl.get() + ARight.get());
          L = ALeft.get().length;
          if (EndsWith(ADelim,ALeft.get())) {
            ARight.set(ADelim + ARight.get());
            $lp2(ALeft,L,1);
          };
        };
        P = $lp4(ADelim,ALeft.get());
        ALvl.set($lp(ALeft.get(),P + 1,2147483647));
        ALeft.set($lp(ALeft.get(),1,P));
      };
      function AddParam(aName, AValue) {
        if (L != null) L.SetValue(aName,AValue);
      };
      var APathInfo = "";
      var APattern = "";
      var VLeftPat = "";
      var VRightPat = "";
      var VLeftVal = "";
      var VRightVal = "";
      var VVal = "";
      var VPat = "";
      var VName = "";
      Result = false;
      if (this.FURLPattern === "") return Result;
      APathInfo = Path;
      APattern = this.FURLPattern;
      $lp2({get: function () {
          return APattern;
        }, set: function (v) {
          APattern = v;
        }},$lp1("?",APattern),2147483647);
      $lp2({get: function () {
          return APathInfo;
        }, set: function (v) {
          APathInfo = v;
        }},$lp1("?",APathInfo),2147483647);
      if (StartsWith("\/",APattern)) $lp2({get: function () {
          return APattern;
        }, set: function (v) {
          APattern = v;
        }},1,1);
      if (StartsWith("\/",APathInfo)) $lp2({get: function () {
          return APathInfo;
        }, set: function (v) {
          APathInfo = v;
        }},1,1);
      VLeftPat = "";
      VLeftVal = "";
      VPat = "\/";
      VVal = "\/";
      VRightPat = APattern;
      VRightVal = APathInfo;
      do {
        ExtractNextPathLevel({get: function () {
            return VLeftPat;
          }, set: function (v) {
            VLeftPat = v;
          }},{get: function () {
            return VPat;
          }, set: function (v) {
            VPat = v;
          }},{get: function () {
            return VRightPat;
          }, set: function (v) {
            VRightPat = v;
          }},"\/");
        ExtractNextPathLevel({get: function () {
            return VLeftVal;
          }, set: function (v) {
            VLeftVal = v;
          }},{get: function () {
            return VVal;
          }, set: function (v) {
            VVal = v;
          }},{get: function () {
            return VRightVal;
          }, set: function (v) {
            VRightVal = v;
          }},"\/");
        if (StartsWith(":",VPat)) {
          AddParam($lp(VPat,2,2147483647),VVal)}
         else if (StartsWith("*",VPat)) {
          VName = $lp(VPat,2,2147483647);
          VLeftPat = VRightPat;
          VLeftVal = VVal + VRightVal;
          VPat = "\/";
          VVal = "\/";
          VRightPat = "";
          VRightVal = "";
          if (EndsWith("\/",VLeftPat) && !EndsWith("\/",VLeftVal)) $lp2({get: function () {
              return VLeftPat;
            }, set: function (v) {
              VLeftPat = v;
            }},VLeftPat.length,1);
          do {
            ExtractPrevPathLevel({get: function () {
                return VLeftPat;
              }, set: function (v) {
                VLeftPat = v;
              }},{get: function () {
                return VPat;
              }, set: function (v) {
                VPat = v;
              }},{get: function () {
                return VRightPat;
              }, set: function (v) {
                VRightPat = v;
              }},"\/");
            ExtractPrevPathLevel({get: function () {
                return VLeftVal;
              }, set: function (v) {
                VLeftVal = v;
              }},{get: function () {
                return VVal;
              }, set: function (v) {
                VVal = v;
              }},{get: function () {
                return VRightVal;
              }, set: function (v) {
                VRightVal = v;
              }},"\/");
            if (StartsWith(":",VPat)) {
              AddParam($lp(VPat,2,2147483647),VVal);
            } else if (!((VPat === "") && (VLeftPat === "")) && (VPat !== VVal)) return Result;
            if ((VLeftPat === "") || (VLeftVal === "")) {
              if (VLeftPat === "") {
                if (VName !== "") AddParam(VName,VLeftVal + VVal);
                Result = true;
              };
              return Result;
            };
          } while (!false);
        } else if (VPat !== VVal) return Result;
        if ((VRightPat === "") || (VRightVal === "")) {
          if ((VRightPat === "") && (VRightVal === "")) {
            Result = true}
           else if (VRightPat === "\/") Result = true;
          return Result;
        };
      } while (!false);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Default",0,rtl.boolean,"FDefault","FDefault");
    $r.addProperty("URLPattern",2,rtl.string,"FURLPattern","SetURLPattern");
    $r.addProperty("Event",0,$mod.$rtti["TRouteEvent"],"FEvent","FEvent");
  });
  rtl.createClass(this,"TRouteList",$lt19,function () {
    $lt9 = this;
    this.GetR = function (AIndex) {
      var Result = null;
      Result = rtl.as(this.GetItem(AIndex),$lt8);
      return Result;
    };
  });
  rtl.recNewT(this,"TScrollParams",function () {
    $lt10 = this;
    this.selector = "";
    this.$new = function () {
      var r = Object.create(this);
      r.Position = $lt1.$new();
      return r;
    };
    this.$eq = function (b) {
      return (this.selector === b.selector) && this.Position.$eq(b.Position);
    };
    this.$assign = function (s) {
      this.selector = s.selector;
      this.Position.$assign(s.Position);
      return this;
    };
  });
  rtl.createClass(this,"TRouter",$lt20,function () {
    $lt11 = this;
    this.FService = null;
    this.FServiceClass = null;
    this.$init = function () {
      $lt20.$init.call(this);
      this.FAfterRequest = null;
      this.FBeforeRequest = null;
      this.FHistory = null;
      this.FOnScroll = null;
      this.FRoutes = null;
    };
    this.$final = function () {
      this.FAfterRequest = undefined;
      this.FBeforeRequest = undefined;
      this.FHistory = undefined;
      this.FOnScroll = undefined;
      this.FRoutes = undefined;
      $lt20.$final.call(this);
    };
    this.GetHistory = function () {
      var Result = null;
      if (this.FHistory === null) this.InitHistory(0,"");
      Result = this.FHistory;
      return Result;
    };
    this.CreateHTTPRoute = function (AClass, APattern, IsDefault) {
      var Result = null;
      this.CheckDuplicate(APattern,IsDefault);
      Result = AClass.$create("Create$1",[this.FRoutes]);
      Result.SetURLPattern(APattern);
      Result.FDefault = IsDefault;
      return Result;
    };
    this.CreateRouteList = function () {
      var Result = null;
      Result = $lt9.$create("Create$1",[$lt8]);
      return Result;
    };
    this.CheckDuplicate = function (APattern, isDefault) {
      var I = 0;
      var DI = 0;
      var R = null;
      DI = -1;
      for (var $l = 0, $end = this.FRoutes.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        R = this.FRoutes.GetR(I);
        if (R.FDefault) DI = I;
        if (R.Matches(APattern)) throw $lt.$create("CreateFmt",[rtl.getResStr($mod,"EDuplicateRoute"),$lp5(18,APattern)]);
      };
      if (isDefault && (DI !== -1)) throw $lt.$create("CreateFmt",[rtl.getResStr($mod,"EDuplicateDefaultRoute"),$lp5(18,APattern)]);
    };
    this.DoRouteRequest = function (ARoute, AURL, AParams) {
      var Result = null;
      Result = ARoute;
      if (Result != null) {
        Result.HandleRequest(this,AURL,AParams)}
       else throw $lt.$create("CreateFmt",["No route for URL: %s",$lp5(18,AURL)]);
      return Result;
    };
    this.DoRouteRequest$1 = function (AURL, DoPush) {
      var Result = null;
      var APath = "";
      var Params = null;
      APath = AURL;
      Params = $lt17.$create("Create$1");
      try {
        Result = this.GetRoute(APath,Params);
        if (DoPush) {
          this.Push(AURL)}
         else Result = this.DoRouteRequest(Result,APath,Params);
      } finally {
        Params = rtl.freeLoc(Params);
      };
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt20.Create$1.call(this,AOwner);
      this.FRoutes = this.CreateRouteList();
      return this;
    };
    this.Destroy = function () {
      $lp6({p: this, get: function () {
          return this.p.FRoutes;
        }, set: function (v) {
          this.p.FRoutes = v;
        }});
      $lt20.Destroy.call(this);
    };
    this.InitHistory = function (aKind, aBase) {
      $lp6({p: this, get: function () {
          return this.p.FHistory;
        }, set: function (v) {
          this.p.FHistory = v;
        }});
      var $tmp = aKind;
      if ($tmp === 1) {
        this.FHistory = $lt5.$create("Create$2",[this,aBase])}
       else if ($tmp === 2) {
        this.FHistory = $lt6.$create("Create$2",[this,aBase])}
       else if ($tmp === 3) {
        this.FHistory = $lt7.$create("Create$2",[this,aBase])}
       else if ($tmp === 0) if ($lt13.supportsPushState()) {
        this.FHistory = $lt7.$create("Create$2",[this,aBase])}
       else this.FHistory = $lt6.$create("Create$2",[this,aBase]);
      this.FHistory.SetupListeners();
    };
    this.SanitizeRoute = function (Path) {
      var Result = "";
      Result = Path;
      return Result;
    };
    this.Service = function () {
      var Result = null;
      if (this.FService === null) $lt11.FService = this.ServiceClass().$create("Create$1",[null]);
      Result = this.FService;
      return Result;
    };
    this.ServiceClass = function () {
      var Result = null;
      if (this.FServiceClass === null) $lt11.FServiceClass = $lt11;
      Result = this.FServiceClass;
      return Result;
    };
    this.RegisterRoute = function (APattern, AEvent, IsDefault) {
      var Result = null;
      Result = this.CreateHTTPRoute($lt14,APattern,IsDefault);
      Result.FEvent = AEvent;
      return Result;
    };
    this.FindHTTPRoute = function (Path, Params) {
      var Result = null;
      var I = 0;
      var APathInfo = "";
      APathInfo = this.$class.SanitizeRoute(Path);
      Result = null;
      I = 0;
      while ((Result === null) && (I < this.FRoutes.GetCount())) {
        Result = this.FRoutes.GetR(I);
        if (!Result.MatchPattern(APathInfo,Params)) Result = null;
        I += 1;
      };
      return Result;
    };
    this.GetRoute = function (Path, Params) {
      var Result = null;
      Result = this.FindHTTPRoute(Path,Params);
      if (!(Result != null)) throw $lt.$create("Create$1",["Not found"]);
      return Result;
    };
    this.RouteRequest = function (ARouteURL, DoPush) {
      var Result = null;
      var AURL = "";
      AURL = ARouteURL;
      if (this.FBeforeRequest != null) this.FBeforeRequest(this,{get: function () {
          return AURL;
        }, set: function (v) {
          AURL = v;
        }});
      Result = this.DoRouteRequest$1(AURL,DoPush);
      if (this.FAfterRequest != null) this.FAfterRequest(this,AURL);
      return Result;
    };
    this.RouteFromURL = function () {
      var Result = "";
      Result = this.GetCurrentLocation();
      if (Result !== "") $mod.Router().RouteRequest(Result,true);
      return Result;
    };
    this.GetCurrentLocation = function () {
      var Result = "";
      Result = this.GetHistory().GetCurrentLocation();
      return Result;
    };
    this.Push = function (location) {
      var Result = 0;
      Result = this.GetHistory().Push(location);
      return Result;
    };
  });
  rtl.createClass(this,"TWebScroll",$lt16,function () {
    $lt12 = this;
    this.scrollToPosition = function (AScroll) {
      var el = null;
      var P = $lt1.$new();
      if (AScroll.selector !== "") {
        el = document.querySelector(AScroll.selector);
        if (el != null) {
          P.$assign($impl.getElementPosition(el,$lt1.$clone(AScroll.Position)))}
         else P.$assign(AScroll.Position);
      } else P.$assign(AScroll.Position);
      window.scrollTo(Math.round(P.X),Math.round(P.Y));
    };
    this.getScrollPosition = function () {
      var Result = $lt1.$new();
      var Key = "";
      var O = undefined;
      Key = this.GetStateKey();
      Result.X = 0;
      Result.Y = 0;
      if (Key !== "") {
        O = $impl.positionStore[Key];
        if (rtl.isObject(O)) {
          Result.X = rtl.getNumber(O["x"]);
          Result.Y = rtl.getNumber(O["y"]);
        };
      };
      return Result;
    };
    this.SaveScrollPosition = function () {
      var Key = "";
      Key = this.GetStateKey();
      if (Key !== "") $impl.positionStore[Key] = $lp7(["x",window.scrollX,"y",window.scrollY]);
    };
    this.Setup = function () {
      window.history.replaceState($lp7(["key",this.GetStateKey()]),"");
      window.addEventListener("popstate",rtl.createSafeCallback($impl,"DoScroll"));
    };
    this.handle = function (router, ato, afrom, isPop) {
      var Position = $lt1.$new();
      var ScrollParams = $lt10.$new();
      if (!(router.FOnScroll != null)) return;
      Position.$assign(this.getScrollPosition());
      ScrollParams.$assign(router.FOnScroll(router,ato,afrom,$lt1.$clone(Position)));
      this.scrollToPosition($lt10.$clone(ScrollParams));
    };
    this.GetStateKey = function () {
      var Result = "";
      Result = (new Date()).toString();
      return Result;
    };
  });
  rtl.createClass(this,"TBrowserState",$lt16,function () {
    $lt13 = this;
    this.TheKey = "";
    this.GenKey = function () {
      var Result = "";
      Result = $lp8(Date.now());
      return Result;
    };
    this.supportsPushState = function () {
      var $Self = this;
      var Result = false;
      var UA = "";
      function isB(B) {
        var Result = false;
        Result = $lp1(B,UA) !== 0;
        return Result;
      };
      Result = false;
      if ($lp9(window) && $lp9(window.navigator)) {
        UA = window.navigator.userAgent;
        Result = !(isB("Android 2.") || isB("Android 4.0") || isB("Mobile Safari") || isB("Chrome") || isB("Windows Phone"));
      };
      if (Result) Result = $lp9(window.history) && $lp9(window.history);
      return Result;
    };
    this.GetStateKey = function () {
      var Result = "";
      if (this.TheKey === "") $lt13.TheKey = this.GenKey();
      Result = this.TheKey;
      return Result;
    };
    this.SetStateKey = function (akey) {
      $lt13.TheKey = akey;
    };
    this.PushState = function (aUrl, replace) {
      var O = null;
      $lt12.SaveScrollPosition();
      try {
        if (!replace) this.SetStateKey(this.GenKey());
        O = $lp7(["key",this.GetStateKey()]);
        if (replace) {
          window.history.replaceState(O,"",aUrl)}
         else window.history.pushState(O,"",aUrl);
      } catch ($e) {
        if (replace) {
          window.location.replace(aUrl)}
         else window.location.assign(aUrl);
      };
    };
    this.ReplaceState = function (aUrl) {
      this.PushState(aUrl,true);
    };
  });
  this.Router = function () {
    var Result = null;
    Result = $lt11.Service();
    return Result;
  };
  this.IncludeHTTPPathDelimiter = function (S) {
    var Result = "";
    if ($lp(S,S.length,1) === "\/") {
      Result = S}
     else Result = S + "\/";
    return Result;
  };
  $mod.$implcode = function () {
    $lm3 = pas.strutils;
    $lp4 = $lm3.RPos;
    $lm4 = pas.JS;
    $lp7 = $lm4.New;
    $lp9 = $lm4.isDefined;
    rtl.createClass($impl,"TRouteEventHandler",$lt8,function () {
      $lt14 = this;
      this.HandleRequest = function (ARouter, URL, Params) {
        if (this.FEvent != null) this.FEvent(URL,this,Params);
      };
    });
    $impl.DoScroll = function (Event) {
      var Result = false;
      $lt12.SaveScrollPosition();
      Result = true;
      return Result;
    };
    $impl.positionStore = null;
    $impl.getElementPosition = function (el, offset) {
      var Result = $lt1.$new();
      var DocEl = null;
      var docRect = null;
      var elRect = null;
      DocEl = document.documentElement;
      docRect = DocEl.getBoundingClientRect();
      elRect = el.getBoundingClientRect();
      Result.X = elRect.left - docRect.left - offset.X;
      Result.Y = elRect.top - docRect.top - offset.Y;
      return Result;
    };
    $mod.$resourcestrings = {EDuplicateRoute: {org: "Duplicate route pattern: %s"}, EDuplicateDefaultRoute: {org: "Duplicate default route registered with pattern: %s"}};
  };
  $mod.$init = function () {
    $impl.positionStore = $lp7([]);
  };
},["strutils","JS"]);
rtl.module("DateUtils",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lm = pas.SysUtils;
  var $lp = $lm.DecodeDate;
  var $lp1 = $lm.FormatDateTime;
  var $lp2 = $lm.Trim;
  var $lp3 = $lm.StrToIntDef;
  var $lm1 = pas.System;
  var $lp4 = $lm1.Copy;
  var $lp5 = $lm.EncodeTime;
  var $lp6 = $lm.EncodeDate;
  var $lt1 = $lm1.TObject;
  var $lp7 = $lm.Format;
  var $lp8 = $lm1.VarRecs;
  var $lp9 = $lm.SameText;
  var $lp10 = $lm.Now;
  var $lp11 = $lm1.upcase;
  var $lt2 = $lm.EConvertError;
  this.YearOf = function (AValue) {
    var Result = 0;
    var D = 0;
    var M = 0;
    $lp(AValue,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }},{get: function () {
        return M;
      }, set: function (v) {
        M = v;
      }});
    return Result;
  };
  this.DateTimeToRFC3339 = function (ADate) {
    var Result = "";
    Result = $lp1('yyyy-mm-dd"T"hh":"nn":"ss"."zzz"Z"',ADate);
    return Result;
  };
  var TPartPos = {"0": "ppTime", ppTime: 0, "1": "ppYear", ppYear: 1, "2": "ppMonth", ppMonth: 2, "3": "ppDay", ppDay: 3, "4": "ppHour", ppHour: 4, "5": "ppMinute", ppMinute: 5, "6": "ppSec", ppSec: 6, "7": "ppMSec", ppMSec: 7};
  var P = [11,1,6,9,12,15,18,21];
  this.TryRFC3339ToDateTime = function (Avalue, ADateTime) {
    var Result = false;
    var lY = 0;
    var lM = 0;
    var lD = 0;
    var lH = 0;
    var lMi = 0;
    var lS = 0;
    var lMs = 0;
    if ($lp2(Avalue) === "") {
      Result = true;
      ADateTime.set(0);
    };
    lY = $lp3($lp4(Avalue,P[1],4),-1);
    lM = $lp3($lp4(Avalue,P[2],2),-1);
    lD = $lp3($lp4(Avalue,P[3],2),-1);
    if (Avalue.length >= P[0]) {
      lH = $lp3($lp4(Avalue,P[4],2),-1);
      lMi = $lp3($lp4(Avalue,P[5],2),-1);
      if (Avalue.length >= P[6]) {
        lS = $lp3($lp4(Avalue,P[6],2),-1)}
       else lS = 0;
      if (Avalue.length >= P[7]) lMs = $lp3($lp4(Avalue,P[7],3),-1);
    } else {
      lH = 0;
      lMi = 0;
      lS = 0;
      lMs = 0;
    };
    Result = (lY >= 0) && (lM >= 0) && (lD >= 0) && (lH >= 0) && (lMi >= 0) && (lS >= 0) && (lMs >= 0);
    if (!Result) {
      ADateTime.set(0)}
     else if ((lY === 0) || (lM === 0) || (lD === 0)) {
      ADateTime.set($lp5(lH,lMi,lS,0))}
     else ADateTime.set($lp6(lY,lM,lD) + $lp5(lH,lMi,lS,lMs));
    return Result;
  };
  rtl.createClass(this,"TDateTimeScanner",$lt1,function () {
    $lt = this;
    this.$init = function () {
      $lt1.$init.call(this);
      this.FPattern = "";
      this.FText = "";
      this.FPatternOffset = 0;
      this.FLen = 0;
      this.FPatternLen = 0;
      this.FPatternPos = 0;
      this.FPos = 0;
      this.FY = 0;
      this.FM = 0;
      this.FD = 0;
      this.FTimeval = 0.0;
    };
    this.ArrayMatchError = function () {
      $impl.raiseexception($lp7($impl.SNoArrayMatch,$lp8(0,this.FPatternPos + 1,0,this.FPos)));
    };
    this.DoDateTime = function () {
      this.MatchPattern($lm.ShortDateFormat);
      this.MatchPattern("\t");
      this.MatchPattern($lm.LongTimeFormat);
      this.FPatternPos += 1;
    };
    this.SetPattern = function (AValue) {
      if (this.FPattern === AValue) return;
      this.FPattern = AValue;
      this.FPatternLen = this.FPattern.length;
    };
    this.SetText = function (AValue) {
      if (this.FText === AValue) return;
      this.FText = AValue;
      this.FLen = this.FText.length;
    };
    this.ScanFixedInt = function (maxv) {
      var Result = 0;
      var c = "";
      var n = "";
      var oi = 0;
      Result = 0;
      oi = this.FPos;
      c = this.FPattern.charAt(this.FPatternPos - 1);
      while ((this.FPatternPos <= this.FPatternLen) && (this.FPattern.charAt(this.FPatternPos - 1) === c)) this.FPatternPos += 1;
      n = this.FText.charAt(this.FPos - 1);
      while ((maxv > 0) && (this.FPos <= this.FLen) && (n.charCodeAt() in rtl.createSet(null,48,57))) {
        Result = ((Result * 10) + n.charCodeAt()) - 48;
        this.FPos += 1;
        maxv -= 1;
        if (this.FPos <= this.FLen) n = this.FText.charAt(this.FPos - 1);
      };
      if (oi === this.FPos) $impl.raiseexception($lp7($impl.SPatternCharMismatch,$lp8(9,c,0,oi)));
      return Result;
    };
    this.ScanPatternLength = function () {
      var Result = 0;
      var c = "";
      var i = 0;
      Result = this.FPatternPos;
      i = this.FPatternPos;
      c = this.FPattern.charAt(i - 1);
      while ((i <= this.FPatternLen) && (this.FPattern.charAt(i - 1) === c)) i += 1;
      Result = i - Result;
      return Result;
    };
    this.MatchChar = function (c) {
      var N = "";
      if (this.FPos <= this.FLen) {
        N = this.FText.charAt(this.FPos - 1)}
       else N = "?";
      if (N !== c) $impl.raiseexception($lp7($impl.SNoCharMatch,$lp8(9,N,9,c,0,this.FPatternPos + this.FPatternOffset,0,this.FPos)));
      this.FPatternPos += 1;
      this.FPos += 1;
    };
    this.FindIMatch = function (values, aTerm) {
      var Result = 0;
      var l = 0;
      var i = 0;
      Result = -1;
      l = rtl.length(values) - 1;
      i = 0;
      while ((i <= l) && (Result === -1)) {
        if ($lp9($lp4(aTerm,1,values[i].length),values[i])) Result = i;
        i += 1;
      };
      return Result;
    };
    this.FindMatch = function (Values) {
      var Result = 0;
      Result = this.FindIMatch(Values,$lp4(this.FText,this.FPos,(this.FLen - this.FPos) + 1));
      if (Result === -1) {
        this.ArrayMatchError()}
       else {
        this.FPos += Values[Result].length + 1;
        this.FPatternPos += Values[Result].length + 1;
        Result += 1;
      };
      return Result;
    };
    this.MatchPattern = function (aPattern) {
      var T = "";
      var cPos = 0;
      T = this.FPattern;
      cPos = this.FPatternPos;
      this.FPatternOffset = this.FPatternPos;
      this.FPattern = aPattern;
      this.FPatternLen = aPattern.length;
      try {
        this.Scan(-1);
      } finally {
        this.FPattern = T;
        this.FPatternLen = aPattern.length;
        this.FPatternPos = cPos;
        this.FPatternOffset = 0;
      };
    };
    this.DoYear = function () {
      var I = 0;
      var pivot = 0;
      I = this.ScanPatternLength();
      this.FY = this.ScanFixedInt(4);
      if (I <= 2) {
        pivot = $mod.YearOf($lp10()) - $lm.TwoDigitYearCenturyWindow;
        this.FY += rtl.trunc(pivot / 100) * 100;
        if (($lm.TwoDigitYearCenturyWindow > 0) && (this.FY < pivot)) this.FY += 100;
      };
    };
    this.DoMonth = function () {
      var I = 0;
      I = this.ScanPatternLength();
      var $tmp = I;
      if (($tmp === 1) || ($tmp === 2)) {
        this.FM = this.ScanFixedInt(2)}
       else if ($tmp === 3) {
        this.FM = this.FindMatch($lm.ShortMonthNames)}
       else if ($tmp === 4) this.FM = this.FindMatch($lm.LongMonthNames);
    };
    this.DoDay = function () {
      var I = 0;
      I = this.ScanPatternLength();
      var $tmp = I;
      if (($tmp === 1) || ($tmp === 2)) {
        this.FD = this.ScanFixedInt(2)}
       else if ($tmp === 3) {
        this.FD = this.FindMatch($lm.ShortDayNames)}
       else if ($tmp === 4) {
        this.FD = this.FindMatch($lm.LongDayNames)}
       else if ($tmp === 5) {
        this.MatchPattern($lm.ShortDateFormat)}
       else if ($tmp === 6) this.MatchPattern($lm.LongDateFormat);
    };
    this.DoTime = function () {
      var I = 0;
      I = this.ScanPatternLength();
      var $tmp = I;
      if ($tmp === 1) {
        this.MatchPattern($lm.ShortTimeFormat)}
       else if ($tmp === 2) this.MatchPattern($lm.LongTimeFormat);
    };
    this.DoAMPM = function () {
      var I = 0;
      I = this.FindIMatch($impl.AMPMformatting,$lp4(this.FPattern,this.FPatternPos,5));
      var $tmp = I;
      if ($tmp === 0) {
        I = this.FindIMatch(["AM","PM"],$lp4(this.FText,this.FPos,2));
        var $tmp1 = I;
        if ($tmp1 === 0) {}
        else if ($tmp1 === 1) {
          this.FTimeval = this.FTimeval + (12 * 0.041666666666666664)}
         else {
          this.ArrayMatchError();
        };
        this.FPatternPos += $impl.AMPMformatting[0].length;
        this.FPos += 2;
      } else if ($tmp === 1) {
        var $tmp2 = $lp11(this.FText.charAt(this.FPos - 1));
        if ($tmp2 === "A") {}
        else if ($tmp2 === "P") {
          this.FTimeval = this.FTimeval + (12 * 0.041666666666666664)}
         else {
          this.ArrayMatchError();
        };
        this.FPatternPos += $impl.AMPMformatting[1].length;
        this.FPos += 1;
      } else if ($tmp === 2) {
        I = this.FindIMatch([$lm.TimeAMString,$lm.TimePMString],$lp4(this.FText,this.FPos,5));
        var $tmp3 = I;
        if ($tmp3 === 0) {
          this.FPos += $lm.TimeAMString.length}
         else if ($tmp3 === 1) {
          this.FTimeval = this.FTimeval + (12 * 0.041666666666666664);
          this.FPos += $lm.TimePMString.length;
        } else {
          this.ArrayMatchError();
        };
        this.FPatternPos += $impl.AMPMformatting[2].length;
        this.FPatternPos += 2;
        this.FPos += 2;
      } else {
        this.MatchChar(this.FPattern.charAt(this.FPatternPos - 1));
      };
    };
    this.Scan = function (StartPos) {
      var Result = 0.0;
      var lasttoken = "";
      var activequote = "";
      var lch = "";
      var i = 0;
      if (StartPos < 1) StartPos = 1;
      if (this.FPos < StartPos) this.FPos = StartPos;
      this.FPatternPos = 1;
      activequote = "\x00";
      lasttoken = " ";
      while ((this.FPos <= this.FLen) && (this.FPatternPos <= this.FPatternLen)) {
        lch = $lp11(this.FPattern.charAt(this.FPatternPos - 1));
        if (activequote !== "\x00") {
          if (activequote !== lch) {
            this.MatchChar(lch)}
           else {
            activequote = "\x00";
            this.FPatternPos += 1;
          };
        } else {
          if ((lch === "M") && (lasttoken === "H")) {
            i = this.ScanPatternLength();
            if (i > 2) $impl.raiseexception($lp7($impl.SHHMMError,$lp8(0,this.FPatternOffset + this.FPatternPos + 1)));
            this.FTimeval = this.FTimeval + (this.ScanFixedInt(2) * 0.00069444444444444447);
          } else {
            var $tmp = lch;
            if ($tmp === "Y") {
              this.DoYear()}
             else if ($tmp === "M") {
              this.DoMonth()}
             else if ($tmp === "D") {
              this.DoDay()}
             else if ($tmp === "T") {
              this.DoTime()}
             else if ($tmp === "H") {
              this.FTimeval = this.FTimeval + (this.ScanFixedInt(2) * 0.041666666666666664)}
             else if ($tmp === "N") {
              this.FTimeval = this.FTimeval + (this.ScanFixedInt(2) * 0.00069444444444444447)}
             else if ($tmp === "S") {
              this.FTimeval = this.FTimeval + (this.ScanFixedInt(2) * 0.000011574074074074073)}
             else if ($tmp === "Z") {
              this.FTimeval = this.FTimeval + (this.ScanFixedInt(3) * 1.1574074074074074E-8)}
             else if ($tmp === "A") {
              this.DoAMPM()}
             else if ($tmp === "\/") {
              this.MatchChar($lm.DateSeparator)}
             else if ($tmp === ":") {
              this.MatchChar($lm.TimeSeparator);
              lch = lasttoken;
            } else if (($tmp === "'") || ($tmp === '"')) {
              activequote = lch;
              this.FPatternPos += 1;
            } else if ($tmp === "C") {
              this.DoDateTime()}
             else if ($tmp === "?") {
              this.FPatternPos += 1;
              this.FPos += 1;
            } else if ($tmp === "\t") {
              while ((this.FPos <= this.FLen) && (this.FText.charCodeAt(this.FPos - 1) in $impl.whitespace)) this.FPos += 1;
              this.FPatternPos += 1;
            } else {
              this.MatchChar(this.FPattern.charAt(this.FPatternPos - 1));
            };
          };
          lasttoken = lch;
        };
      };
      Result = this.FTimeval;
      if ((this.FY > 0) && (this.FM > 0) && (this.FD > 0)) Result = Result + $lp6(this.FY,this.FM,this.FD);
      return Result;
    };
  });
  this.ScanDateTime = function (APattern, AValue, APos) {
    var Result = 0.0;
    var T = null;
    T = $lt.$create("Create");
    try {
      T.SetPattern(APattern);
      T.SetText(AValue);
      Result = T.Scan(APos);
    } finally {
      T = rtl.freeLoc(T);
    };
    return Result;
  };
  $mod.$implcode = function () {
    $impl.whitespace = rtl.createSet(32,13,10);
    $impl.hrfactor = 1 / 24;
    $impl.minfactor = 1 / (24 * 60);
    $impl.secfactor = 1 / (24 * 60 * 60);
    $impl.mssecfactor = 1 / (24 * 60 * 60 * 1000);
    $impl.AMPMformatting = ["am\/pm","a\/p","ampm"];
    $impl.raiseexception = function (s) {
      throw $lt2.$create("Create$1",[s]);
    };
    $impl.SPatternCharMismatch = 'Pattern mismatch char "%s" at position %d.';
    $impl.SNoCharMatch = 'Mismatch char "%s" <> "%s" at pattern position %d, string position %d.';
    $impl.SHHMMError = "mm in a sequence hh:mm is interpreted as minutes. No longer versions allowed! (Position : %d).";
    $impl.SNoArrayMatch = "Can't match any allowed value at pattern position %d, string position %d.";
  };
},["JS","RTLConsts"]);
rtl.module("DBConst",["System"],function () {
  "use strict";
  var $mod = this;
  $mod.$resourcestrings = {SActiveDataset: {org: "Operation cannot be performed on an active dataset"}, SDuplicateFieldName: {org: 'Duplicate fieldname : "%s"'}, SFieldNotFound: {org: 'Field not found : "%s"'}, SInactiveDataset: {org: "Operation cannot be performed on an inactive dataset"}, SInvalidDisplayValues: {org: '"%s" are not valid boolean displayvalues'}, SInvalidFieldSize: {org: "Invalid field size : %d"}, SInvalidTypeConversion: {org: "Invalid type conversion to %s in field %s"}, SNeedField: {org: "Field %s is required, but not supplied."}, SNeedFieldName: {org: "Field needs a name"}, SNoDataset: {org: 'No dataset asssigned for field : "%s"'}, SNoSuchRecord: {org: "Could not find the requested record."}, SNotEditing: {org: 'Operation not allowed, dataset "%s" is not in an edit or insert state.'}, SRangeError: {org: "%f is not between %f and %f for %s"}, SUniDirectional: {org: "Operation cannot be performed on an unidirectional dataset"}, SUnknownFieldType: {org: "Unknown field type : %s"}, SIndexNotFound: {org: "Index '%s' not found"}, SFieldValueError: {org: "Invalid value for field '%s'"}, SDuplicateName: {org: "Duplicate name '%s' in %s"}, SLookupInfoError: {org: "Lookup information for field '%s' is incomplete"}, SErrInvalidDateTime: {org: 'Invalid date\/time value : "%s"'}, SErrInsertingSameRecordtwice: {org: "Attempt to insert the same record twice."}, SNestedDataSetClass: {org: "Nested dataset must inherit from %s"}, SCircularDataLink: {org: "Circular datalinks are not allowed"}};
});
rtl.module("DB",["System","Classes","SysUtils","JS","Types","DateUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lt15 = null;
  var $lt16 = null;
  var $lt17 = null;
  var $lt18 = null;
  var $lt19 = null;
  var $lt20 = null;
  var $lt21 = null;
  var $lt22 = null;
  var $lt23 = null;
  var $lt24 = null;
  var $lt25 = null;
  var $lt26 = null;
  var $lt27 = null;
  var $lt28 = null;
  var $lt29 = null;
  var $lt30 = null;
  var $lt31 = null;
  var $lt32 = null;
  var $lt33 = null;
  var $lt34 = null;
  var $lt35 = null;
  var $lt36 = null;
  var $lt37 = null;
  var $lt38 = null;
  var $lt39 = null;
  var $lt40 = null;
  var $lt41 = null;
  var $lt42 = null;
  var $lt43 = null;
  var $lt44 = null;
  var $lt45 = null;
  var $lt46 = null;
  var $lt47 = null;
  var $lt48 = null;
  var $lt49 = null;
  var $lt50 = null;
  var $lt51 = null;
  var $lt52 = null;
  var $lm = pas.SysUtils;
  var $lt53 = $lm.Exception;
  var $lm1 = pas.Classes;
  var $lt54 = $lm1.TCollectionItem;
  var $lm2 = null;
  var $lm3 = pas.System;
  var $lp = $lm3.VarRecs;
  var $lt55 = $lm1.TOwnedCollection;
  var $lp1 = $lm3.Copy;
  var $lp2 = $lm.IntToStr;
  var $lt56 = $lm1.TCollection;
  var $lp3 = $lm.AnsiSameText;
  var $lt57 = $lm1.TPersistent;
  var $lt58 = $lm3.TObject;
  var $lt59 = $lm1.TFPList;
  var $lt60 = $lm1.TComponent;
  var $lp4 = $lm3.Pos;
  var $lp5 = $lm.LowerCase;
  var $lp6 = $lm.UpperCase;
  var $lm4 = pas.JS;
  var $lp7 = $lm4.isNull;
  var $lt61 = $lm.EVariantError;
  var $lp8 = $lm4.isUndefined;
  var $lt62 = $lm1.TAlignment;
  var $lt63 = $lt62.taLeftJustify;
  var $lp9 = $lm3.upcase;
  var $lp10 = $lm.StrToDateTime;
  var $lp11 = $lm.StrToFloat;
  var $lp12 = $lm.StrToInt;
  var $lp13 = $lm.StrToInt64;
  var $lt64 = $lt62.taRightJustify;
  var $lp14 = $lm4.isInteger;
  var $lp15 = $lm.FloatToStr;
  var $lp16 = $lm4.isBoolean;
  var $lp17 = $lm.FormatDateTime;
  var $lp18 = $lm3.Assigned;
  var $lp19 = $lm.FreeAndNil;
  var $lp20 = $lm.Abort;
  var $lt65 = $lm1.TList;
  var $lm5 = pas.DateUtils;
  var $lp21 = $lm5.TryRFC3339ToDateTime;
  var $lt66 = $lm.EConvertError;
  var $lp22 = $lm4.isDate;
  var $lp23 = $lm.JSDateToDateTime;
  var $lp24 = $lm5.DateTimeToRFC3339;
  var $lp25 = $lm.IntToHex;
  var $lp26 = $lm.Format;
  var $lp27 = $lm.Trim;
  this.YesNoChars = ["N","Y"];
  $lt = this.TDataSetState = {"0": "dsInactive", dsInactive: 0, "1": "dsBrowse", dsBrowse: 1, "2": "dsEdit", dsEdit: 2, "3": "dsInsert", dsInsert: 3, "4": "dsSetKey", dsSetKey: 4, "5": "dsCalcFields", dsCalcFields: 5, "6": "dsFilter", dsFilter: 6, "7": "dsNewValue", dsNewValue: 7, "8": "dsOldValue", dsOldValue: 8, "9": "dsCurValue", dsCurValue: 9, "10": "dsBlockRead", dsBlockRead: 10, "11": "dsInternalCalc", dsInternalCalc: 11, "12": "dsOpening", dsOpening: 12, "13": "dsRefreshFields", dsRefreshFields: 13};
  $lt1 = this.TDataEvent = {"0": "deFieldChange", deFieldChange: 0, "1": "deRecordChange", deRecordChange: 1, "2": "deDataSetChange", deDataSetChange: 2, "3": "deDataSetScroll", deDataSetScroll: 3, "4": "deLayoutChange", deLayoutChange: 4, "5": "deUpdateRecord", deUpdateRecord: 5, "6": "deUpdateState", deUpdateState: 6, "7": "deCheckBrowseMode", deCheckBrowseMode: 7, "8": "dePropertyChange", dePropertyChange: 8, "9": "deFieldListChange", deFieldListChange: 9, "10": "deFocusControl", deFocusControl: 10, "11": "deParentScroll", deParentScroll: 11, "12": "deConnectChange", deConnectChange: 12, "13": "deReconcileError", deReconcileError: 13, "14": "deDisabledStateChange", deDisabledStateChange: 14};
  $lt2 = this.TUpdateStatus = {"0": "usModified", usModified: 0, "1": "usInserted", usInserted: 1, "2": "usDeleted", usDeleted: 2};
  $lt3 = this.TProviderFlag = {"0": "pfInUpdate", pfInUpdate: 0, "1": "pfInWhere", pfInWhere: 1, "2": "pfInKey", pfInKey: 2, "3": "pfHidden", pfHidden: 3, "4": "pfRefreshOnInsert", pfRefreshOnInsert: 4, "5": "pfRefreshOnUpdate", pfRefreshOnUpdate: 5};
  this.$rtti.$Enum("TProviderFlag",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TProviderFlag});
  this.$rtti.$Set("TProviderFlags",{comptype: this.$rtti["TProviderFlag"]});
  this.$rtti.$Class("TFieldDefs");
  this.$rtti.$Class("TField");
  this.$rtti.$Class("TDataSet");
  this.$rtti.$Class("TDataSource");
  rtl.createClass(this,"EDatabaseError",$lt53,function () {
    $lt4 = this;
  });
  $lt5 = this.TFieldType = {"0": "ftUnknown", ftUnknown: 0, "1": "ftString", ftString: 1, "2": "ftInteger", ftInteger: 2, "3": "ftLargeInt", ftLargeInt: 3, "4": "ftBoolean", ftBoolean: 4, "5": "ftFloat", ftFloat: 5, "6": "ftDate", ftDate: 6, "7": "ftTime", ftTime: 7, "8": "ftDateTime", ftDateTime: 8, "9": "ftAutoInc", ftAutoInc: 9, "10": "ftBlob", ftBlob: 10, "11": "ftMemo", ftMemo: 11, "12": "ftFixedChar", ftFixedChar: 12, "13": "ftVariant", ftVariant: 13, "14": "ftDataset", ftDataset: 14};
  this.$rtti.$Enum("TFieldType",{minvalue: 0, maxvalue: 14, ordtype: 1, enumtype: this.TFieldType});
  $lt6 = this.TFieldAttribute = {"0": "faHiddenCol", faHiddenCol: 0, "1": "faReadonly", faReadonly: 1, "2": "faRequired", faRequired: 2, "3": "faLink", faLink: 3, "4": "faUnNamed", faUnNamed: 4, "5": "faFixed", faFixed: 5};
  this.$rtti.$Enum("TFieldAttribute",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TFieldAttribute});
  this.$rtti.$Set("TFieldAttributes",{comptype: this.$rtti["TFieldAttribute"]});
  rtl.createClass(this,"TNamedItem",$lt54,function () {
    $lt7 = this;
    this.$init = function () {
      $lt54.$init.call(this);
      this.FName = "";
    };
    this.GetDisplayName = function () {
      var Result = "";
      Result = this.FName;
      return Result;
    };
    this.SetDisplayName = function (Value) {
      var TmpInd = 0;
      if (this.FName === Value) return;
      if ((Value !== "") && $lt10.isPrototypeOf(this.FCollection)) {
        TmpInd = this.FCollection.IndexOf(Value);
        if ((TmpInd >= 0) && (TmpInd !== this.GetIndex())) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SDuplicateName"),$lp(18,Value,18,this.FCollection.$classname));
      };
      this.FName = Value;
      $lt54.SetDisplayName.call(this,Value);
    };
    var $r = this.$rtti;
    $r.addProperty("Name",2,rtl.string,"FName","SetDisplayName");
  });
  rtl.createClass(this,"TDefCollection",$lt55,function () {
    $lt8 = this;
    this.$init = function () {
      $lt55.$init.call(this);
      this.FDataset = null;
    };
    this.$final = function () {
      this.FDataset = undefined;
      $lt55.$final.call(this);
    };
    this.SetItemName = function (Item) {
      var N = null;
      var TN = "";
      N = rtl.as(Item,$lt7);
      if (N.FName === "") {
        TN = $lp1(this.$classname,2,5) + $lp2(N.FID + 1);
        if (this.FDataset != null) TN = this.FDataset.FName + TN;
        N.SetDisplayName(TN);
      } else $lt56.SetItemName.call(this,Item);
    };
    this.create$3 = function (ADataset, AOwner, AClass) {
      $lt55.Create$2.call(this,AOwner,AClass);
      this.FDataset = ADataset;
      return this;
    };
    this.Find = function (AName) {
      var Result = null;
      var i = 0;
      Result = null;
      for (var $l = 0, $end = this.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        if ($lp3(this.GetItem(i).FName,AName)) {
          Result = this.GetItem(i);
          break;
        };
      };
      return Result;
    };
    this.IndexOf = function (AName) {
      var Result = 0;
      var i = 0;
      Result = -1;
      for (var $l = 0, $end = this.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        if ($lp3(this.GetItem(i).FName,AName)) {
          Result = i;
          break;
        };
      };
      return Result;
    };
  });
  rtl.createClass(this,"TFieldDef",$lt7,function () {
    $lt9 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FAttributes = {};
      this.FDataType = 0;
      this.FFieldNo = 0;
      this.FInternalCalcField = false;
      this.FPrecision = 0;
      this.FRequired = false;
      this.FSize = 0;
    };
    this.$final = function () {
      this.FAttributes = undefined;
      $lt7.$final.call(this);
    };
    this.GetFieldClass = function () {
      var Result = null;
      if ((this.FCollection != null) && $lt10.isPrototypeOf(this.FCollection) && (this.FCollection.FDataset != null)) {
        Result = this.FCollection.FDataset.GetFieldClass(this.FDataType)}
       else Result = null;
      return Result;
    };
    this.SetAttributes = function (AValue) {
      this.FAttributes = rtl.refSet(AValue);
      this.Changed(false);
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
      this.Changed(false);
    };
    this.SetPrecision = function (AValue) {
      this.FPrecision = AValue;
      this.Changed(false);
    };
    this.SetSize = function (AValue) {
      this.FSize = AValue;
      this.Changed(false);
    };
    this.Create$1 = function (ACollection) {
      $lt54.Create$1.call(this,ACollection);
      this.FFieldNo = this.GetIndex() + 1;
      return this;
    };
    this.Create$3 = function (AOwner, AName, ADataType, ASize, ARequired, AFieldNo) {
      $lt54.Create$1.call(this,AOwner);
      this.SetDisplayName(AName);
      this.FDataType = ADataType;
      this.FSize = ASize;
      this.FRequired = ARequired;
      this.FPrecision = -1;
      this.FFieldNo = AFieldNo;
      return this;
    };
    this.Destroy = function () {
      $lt54.Destroy.call(this);
    };
    this.Assign = function (Source) {
      var fd = null;
      fd = null;
      if ($lt9.isPrototypeOf(Source)) fd = rtl.as(Source,$lt9);
      if (fd != null) {
        this.FCollection.BeginUpdate();
        try {
          this.SetDisplayName(fd.FName);
          this.SetDataType(fd.FDataType);
          this.SetSize(fd.FSize);
          this.SetPrecision(fd.FPrecision);
          this.FRequired = fd.FRequired;
        } finally {
          this.FCollection.EndUpdate();
        };
      } else $lt57.Assign.call(this,Source);
    };
    this.CreateField = function (AOwner) {
      var Result = null;
      var TheField = null;
      TheField = this.GetFieldClass();
      if (TheField === null) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SUnknownFieldType"),$lp(18,this.FName));
      Result = TheField.$create("Create$1",[AOwner]);
      try {
        Result.FFieldDef = this;
        Result.SetSize(this.FSize);
        Result.FRequired = this.FRequired;
        Result.FFieldName = this.FName;
        Result.FDisplayLabel = this.GetDisplayName();
        Result.FFieldNo = this.FFieldNo;
        Result.SetFieldType(this.FDataType);
        Result.FReadOnly = 1 in this.FAttributes;
        Result.SetDataset(this.FCollection.FDataset);
        if ($lt19.isPrototypeOf(Result)) Result.SetPrecision(this.FPrecision);
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Attributes",2,$mod.$rtti["TFieldAttributes"],"FAttributes","SetAttributes",{Default: {}});
    $r.addProperty("DataType",2,$mod.$rtti["TFieldType"],"FDataType","SetDataType");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 0});
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass(this,"TFieldDefs",$lt8,function () {
    $lt10 = this;
    this.GetItem$1 = function (Index) {
      var Result = null;
      Result = this.GetItem(Index);
      return Result;
    };
    this.FieldDefClass = function () {
      var Result = null;
      Result = $lt9;
      return Result;
    };
    this.Create$4 = function (ADataSet) {
      $lt8.create$3.call(this,ADataSet,this.Owner(),this.$class.FieldDefClass());
      return this;
    };
    this.Add$2 = function (AName, ADataType, ASize, ARequired, AFieldNo) {
      var Result = null;
      Result = this.$class.FieldDefClass().$create("Create$3",[this,AName,ADataType,ASize,ARequired,AFieldNo]);
      return Result;
    };
    this.Add$3 = function (AName, ADataType, ASize, ARequired) {
      if (AName.length === 0) $mod.DatabaseError$1(rtl.getResStr($lm2,"SNeedFieldName"),this.FDataset);
      this.BeginUpdate();
      try {
        this.Add$2(AName,ADataType,ASize,ARequired,this.GetCount() + 1);
      } finally {
        this.EndUpdate();
      };
    };
    this.Add$4 = function (AName, ADataType, ASize) {
      this.Add$3(AName,ADataType,ASize,false);
    };
    this.Assign$2 = function (FieldDefs) {
      var I = 0;
      this.Clear();
      for (var $l = 0, $end = FieldDefs.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        var $with = FieldDefs.GetItem$1(I);
        this.Add$3($with.FName,$with.FDataType,$with.FSize,$with.FRequired);
      };
    };
  });
  $lt11 = this.TFieldKind = {"0": "fkData", fkData: 0, "1": "fkCalculated", fkCalculated: 1, "2": "fkLookup", fkLookup: 2, "3": "fkInternalCalc", fkInternalCalc: 3};
  this.$rtti.$Enum("TFieldKind",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TFieldKind});
  this.$rtti.$MethodVar("TFieldNotifyEvent",{procsig: rtl.newTIProcSig([["Sender",this.$rtti["TField"]]]), methodkind: 0});
  this.$rtti.$MethodVar("TFieldGetTextEvent",{procsig: rtl.newTIProcSig([["Sender",this.$rtti["TField"]],["aText",rtl.string,1],["DisplayText",rtl.boolean]]), methodkind: 0});
  this.$rtti.$MethodVar("TFieldSetTextEvent",{procsig: rtl.newTIProcSig([["Sender",this.$rtti["TField"]],["aText",rtl.string,2]]), methodkind: 0});
  rtl.createClass(this,"TLookupList",$lt58,function () {
    $lt12 = this;
    this.$init = function () {
      $lt58.$init.call(this);
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      $lt58.$final.call(this);
    };
    this.Create$1 = function () {
      this.FList = $lt59.$create("Create");
      return this;
    };
    this.Destroy = function () {
      this.Clear();
      this.FList.$destroy("Destroy");
      $lt58.Destroy.call(this);
    };
    this.Clear = function () {
      this.FList.Clear();
    };
  });
  rtl.createClass(this,"TField",$lt60,function () {
    $lt13 = this;
    this.$init = function () {
      $lt60.$init.call(this);
      this.FAlignment = 0;
      this.FConstraintErrorMessage = "";
      this.FCustomConstraint = "";
      this.FDataSet = null;
      this.FDataType = 0;
      this.FDefaultExpression = "";
      this.FDisplayLabel = "";
      this.FDisplayWidth = 0;
      this.FFieldDef = null;
      this.FFieldKind = 0;
      this.FFieldName = "";
      this.FFieldNo = 0;
      this.FFields = null;
      this.FHasConstraints = false;
      this.FImportedConstraint = "";
      this.FKeyFields = "";
      this.FLookupCache = false;
      this.FLookupDataSet = null;
      this.FLookupKeyfields = "";
      this.FLookupresultField = "";
      this.FLookupList = null;
      this.FOnChange = null;
      this.FOnGetText = null;
      this.FOnSetText = null;
      this.FOnValidate = null;
      this.FOrigin = "";
      this.FReadOnly = false;
      this.FRequired = false;
      this.FSize = 0;
      this.FValidChars = [];
      this.FValueBuffer = undefined;
      this.FValidating = false;
      this.FVisible = false;
      this.FProviderFlags = {};
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FFieldDef = undefined;
      this.FFields = undefined;
      this.FLookupDataSet = undefined;
      this.FLookupList = undefined;
      this.FOnChange = undefined;
      this.FOnGetText = undefined;
      this.FOnSetText = undefined;
      this.FOnValidate = undefined;
      this.FValidChars = undefined;
      this.FProviderFlags = undefined;
      $lt60.$final.call(this);
    };
    this.GetIndex = function () {
      var Result = 0;
      if (this.FDataSet != null) {
        Result = this.FDataSet.FFieldList.IndexOf(this)}
       else Result = -1;
      return Result;
    };
    this.SetAlignment = function (AValue) {
      if (this.FAlignment !== AValue) {
        this.FAlignment = AValue;
        this.PropertyChanged(false);
      };
    };
    this.SetIndex = function (AValue) {
      if (this.FFields !== null) this.FFields.SetFieldIndex(this,AValue);
    };
    this.SetDisplayLabel = function (AValue) {
      if (this.FDisplayLabel !== AValue) {
        this.FDisplayLabel = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetDisplayWidth = function (AValue) {
      if (this.FDisplayWidth !== AValue) {
        this.FDisplayWidth = AValue;
        this.PropertyChanged(true);
      };
    };
    this.GetDisplayWidth = function () {
      var Result = 0;
      if (this.FDisplayWidth === 0) {
        Result = this.GetDefaultWidth()}
       else Result = this.FDisplayWidth;
      return Result;
    };
    this.SetReadOnly = function (AValue) {
      if (this.FReadOnly !== AValue) {
        this.FReadOnly = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetVisible = function (AValue) {
      if (this.FVisible !== AValue) {
        this.FVisible = AValue;
        this.PropertyChanged(true);
      };
    };
    this.IsDisplayLabelStored = function () {
      var Result = false;
      Result = this.GetDisplayName() !== this.FFieldName;
      return Result;
    };
    this.IsDisplayWidthStored = function () {
      var Result = false;
      Result = this.FDisplayWidth !== 0;
      return Result;
    };
    this.GetLookupList = function () {
      var Result = null;
      if (!(this.FLookupList != null)) this.FLookupList = $lt12.$create("Create$1");
      Result = this.FLookupList;
      return Result;
    };
    this.CalcLookupValue = function () {
      if ((this.FLookupDataSet != null) && this.FLookupDataSet.GetActive()) {
        this.SetAsJSValue(this.FLookupDataSet.Lookup(this.FLookupKeyfields,this.FDataSet.GetFieldValues(this.FKeyFields),this.FLookupresultField))}
       else this.SetAsJSValue(null);
    };
    this.RaiseAccessError = function (TypeName) {
      var E = null;
      E = this.AccessError(TypeName);
      throw E;
    };
    this.AccessError = function (TypeName) {
      var Result = null;
      Result = $lt4.$create("CreateFmt",[rtl.getResStr($lm2,"SInvalidTypeConversion"),$lp(18,TypeName,18,this.FFieldName)]);
      return Result;
    };
    this.CheckInactive = function () {
      if (this.FDataSet != null) this.FDataSet.CheckInactive();
    };
    this.CheckTypeSize = function (AValue) {
      if ((AValue !== 0) && !this.IsBlob()) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidFieldSize"),$lp(0,AValue));
    };
    this.Change = function () {
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.Bind = function (Binding) {
      if (Binding && (this.FFieldKind === 2)) {
        if ((this.FLookupDataSet === null) || (this.FLookupKeyfields === "") || (this.FLookupresultField === "") || (this.FKeyFields === "")) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SLookupInfoError"),$lp(18,this.GetDisplayName()));
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.Open();
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        if (this.FLookupCache) this.RefreshLookupList();
      };
    };
    this.GetAsBoolean = function () {
      var Result = false;
      this.RaiseAccessError($impl.SBoolean);
      Result = false;
      return Result;
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      this.RaiseAccessError($impl.SLargeInt);
      Result = 0;
      return Result;
    };
    this.GetAsDateTime = function () {
      var Result = 0.0;
      this.RaiseAccessError($impl.SDateTime);
      Result = 0.0;
      return Result;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      this.RaiseAccessError($impl.SDateTime);
      Result = 0.0;
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      this.RaiseAccessError($impl.SInteger);
      Result = 0;
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      Result = this.GetClassDesc();
      return Result;
    };
    this.GetClassDesc = function () {
      var Result = "";
      var ClassN = "";
      ClassN = $lp1(this.$classname,2,$lp4("Field",this.$classname) - 2);
      if (this.GetIsNull()) {
        Result = "(" + $lp5(ClassN) + ")"}
       else Result = "(" + $lp6(ClassN) + ")";
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = 10;
      return Result;
    };
    this.GetDisplayName = function () {
      var Result = "";
      if (this.FDisplayLabel !== "") {
        Result = this.FDisplayLabel}
       else Result = this.FFieldName;
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      Result = $lp7(this.GetData());
      return Result;
    };
    this.Notification = function (AComponent, Operation) {
      $lt60.Notification.call(this,AComponent,Operation);
      if ((Operation === 1) && (AComponent === this.FLookupDataSet)) this.FLookupDataSet = null;
    };
    this.PropertyChanged = function (LayoutAffected) {
      if ((this.FDataSet !== null) && this.FDataSet.GetActive()) if (LayoutAffected) {
        this.FDataSet.DataEvent(4,0)}
       else this.FDataSet.DataEvent(2,0);
    };
    this.SetAsJSValue = function (AValue) {
      if ($lp7(AValue)) {
        this.Clear()}
       else try {
        this.SetVarValue(AValue);
      } catch ($e) {
        if ($lt61.isPrototypeOf($e)) {
          $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SFieldValueError"),$lp(18,this.GetDisplayName()))}
         else throw $e
      };
    };
    this.SetDataset = function (AValue) {
      if (AValue === this.FDataSet) return;
      if (this.FDataSet != null) {
        this.FDataSet.CheckInactive();
        this.FDataSet.FFieldList.Remove(this);
      };
      if (AValue != null) {
        AValue.CheckInactive();
        AValue.FFieldList.Add(this);
      };
      this.FDataSet = AValue;
    };
    this.SetDataType = function (AValue) {
      this.FDataType = AValue;
    };
    this.SetSize = function (AValue) {
      this.CheckInactive();
      this.$class.CheckTypeSize(AValue);
      this.FSize = AValue;
    };
    this.SetVarValue = function (AValue) {
      this.RaiseAccessError($impl.SJSValue);
    };
    this.Create$1 = function (AOwner) {
      $lt60.Create$1.call(this,AOwner);
      this.FVisible = true;
      this.FValidChars = rtl.arraySetLength(this.FValidChars,"",255);
      this.FProviderFlags = rtl.createSet(0,1);
      return this;
    };
    this.Destroy = function () {
      if (this.FDataSet != null) {
        this.FDataSet.SetActive(false);
        if (this.FFields != null) this.FFields.Remove(this);
      };
      rtl.free(this,"FLookupList");
      $lt60.Destroy.call(this);
    };
    this.Assign = function (Source) {
      if (Source === null) {
        this.Clear()}
       else if ($lt13.isPrototypeOf(Source)) {
        this.SetAsJSValue(Source.GetAsJSValue());
      } else $lt57.Assign.call(this,Source);
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.GetData = function () {
      var Result = undefined;
      if (this.FDataSet === null) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SNoDataset"),$lp(18,this.FFieldName));
      if (this.FValidating) {
        Result = this.FValueBuffer}
       else {
        Result = this.FDataSet.GetFieldData(this);
        if ($lp8(Result)) Result = null;
      };
      return Result;
    };
    this.IsBlob = function () {
      var Result = false;
      Result = false;
      return Result;
    };
    this.RefreshLookupList = function () {
      var tmpActive = false;
      if (!(this.FLookupDataSet != null) || (this.FLookupKeyfields.length === 0) || (this.FLookupresultField.length === 0) || (this.FKeyFields.length === 0)) return;
      tmpActive = this.FLookupDataSet.GetActive();
      try {
        this.FLookupDataSet.SetActive(true);
        this.FFields.CheckFieldNames(this.FKeyFields);
        this.FLookupDataSet.FFieldList.CheckFieldNames(this.FLookupKeyfields);
        this.FLookupDataSet.FieldByName(this.FLookupresultField);
        this.GetLookupList().Clear();
        this.FLookupDataSet.DisableControls();
        try {
          this.FLookupDataSet.First();
          while (!this.FLookupDataSet.FEOF) {
            this.FLookupDataSet.Next();
          };
        } finally {
          this.FLookupDataSet.EnableControls();
        };
      } finally {
        this.FLookupDataSet.SetActive(tmpActive);
      };
    };
    this.SetData = function (Buffer) {
      if (!(this.FDataSet != null)) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SNoDataset"),$lp(18,this.FFieldName));
      this.FDataSet.SetFieldData(this,Buffer);
    };
    this.SetFieldType = function (AValue) {
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,$lm1.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: $lt63});
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ConstraintErrorMessage",0,rtl.string,"FConstraintErrorMessage","FConstraintErrorMessage");
    $r.addProperty("DefaultExpression",0,rtl.string,"FDefaultExpression","FDefaultExpression");
    $r.addProperty("DisplayLabel",15,rtl.string,"GetDisplayName","SetDisplayLabel",{stored: "IsDisplayLabelStored"});
    $r.addProperty("DisplayWidth",15,rtl.longint,"GetDisplayWidth","SetDisplayWidth",{stored: "IsDisplayWidthStored"});
    $r.addProperty("FieldKind",0,$mod.$rtti["TFieldKind"],"FFieldKind","FFieldKind");
    $r.addProperty("FieldName",0,rtl.string,"FFieldName","FFieldName");
    $r.addProperty("HasConstraints",0,rtl.boolean,"FHasConstraints","");
    $r.addProperty("Index",3,rtl.longint,"GetIndex","SetIndex");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
    $r.addProperty("KeyFields",0,rtl.string,"FKeyFields","FKeyFields");
    $r.addProperty("LookupCache",0,rtl.boolean,"FLookupCache","FLookupCache");
    $r.addProperty("LookupDataSet",0,$mod.$rtti["TDataSet"],"FLookupDataSet","FLookupDataSet");
    $r.addProperty("LookupKeyFields",0,rtl.string,"FLookupKeyfields","FLookupKeyfields");
    $r.addProperty("LookupResultField",0,rtl.string,"FLookupresultField","FLookupresultField");
    $r.addProperty("Origin",0,rtl.string,"FOrigin","FOrigin");
    $r.addProperty("ProviderFlags",0,$mod.$rtti["TProviderFlags"],"FProviderFlags","FProviderFlags");
    $r.addProperty("ReadOnly",2,rtl.boolean,"FReadOnly","SetReadOnly");
    $r.addProperty("Required",0,rtl.boolean,"FRequired","FRequired");
    $r.addProperty("Visible",2,rtl.boolean,"FVisible","SetVisible",{Default: true});
    $r.addProperty("OnChange",0,$mod.$rtti["TFieldNotifyEvent"],"FOnChange","FOnChange");
    $r.addProperty("OnGetText",0,$mod.$rtti["TFieldGetTextEvent"],"FOnGetText","FOnGetText");
    $r.addProperty("OnSetText",0,$mod.$rtti["TFieldSetTextEvent"],"FOnSetText","FOnSetText");
    $r.addProperty("OnValidate",0,$mod.$rtti["TFieldNotifyEvent"],"FOnValidate","FOnValidate");
  });
  rtl.createClass(this,"TStringField",$lt13,function () {
    $lt14 = this;
    this.$init = function () {
      $lt13.$init.call(this);
      this.FFixedChar = false;
      this.FTransliterate = false;
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue < 0) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidFieldSize"),$lp(0,AValue));
    };
    this.GetAsBoolean = function () {
      var Result = false;
      var S = "";
      S = this.GetAsString();
      Result = (S.length > 0) && ($lp9(S.charAt(0)).charCodeAt() in rtl.createSet(84,$mod.YesNoChars[1].charCodeAt()));
      return Result;
    };
    this.GetAsDateTime = function () {
      var Result = 0.0;
      Result = $lp10(this.GetAsString());
      return Result;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      Result = $lp11(this.GetAsString());
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = $lp12(this.GetAsString());
      return Result;
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      Result = $lp13(this.GetAsString());
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if (rtl.isString(V)) {
        Result = "" + V}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FSize;
      return Result;
    };
    this.SetAsString = function (AValue) {
      this.SetData(AValue);
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError(rtl.getResStr($lm2,"SFieldValueError"));
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      this.SetDataType(1);
      this.FFixedChar = false;
      this.FTransliterate = false;
      this.FSize = 20;
      return this;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in rtl.createSet(1,12)) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 20});
  });
  rtl.createClass(this,"TNumericField",$lt13,function () {
    $lt15 = this;
    this.$init = function () {
      $lt13.$init.call(this);
      this.FDisplayFormat = "";
      this.FEditFormat = "";
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue > 16) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidFieldSize"),$lp(0,AValue));
    };
    this.RangeError = function (AValue, Min, Max) {
      $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SRangeError"),$lp(3,AValue,3,Min,3,Max,18,this.FFieldName));
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.SetEditFormat = function (AValue) {
      if (this.FEditFormat !== AValue) {
        this.FEditFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.GetAsBoolean = function () {
      var Result = false;
      Result = this.GetAsInteger() !== 0;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      this.SetAlignment(1);
      return this;
    };
    var $r = this.$rtti;
    $r.addProperty("Alignment",2,$lm1.$rtti["TAlignment"],"FAlignment","SetAlignment",{Default: $lt64});
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
    $r.addProperty("EditFormat",2,rtl.string,"FEditFormat","SetEditFormat");
  });
  rtl.createClass(this,"TIntegerField",$lt15,function () {
    $lt16 = this;
    this.$init = function () {
      $lt15.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      Result = this.GetAsInteger();
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      if (!this.GetValue({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = 0;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = $lp2(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var V = undefined;
      V = this.GetData();
      Result = $lp14(V);
      if (Result) AValue.set(rtl.trunc(V));
      return Result;
    };
    this.SetAsInteger = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        this.RangeError(AValue,this.FMinValue,this.FMaxValue)}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetVarValue = function (AValue) {
      if ($lp14(AValue)) {
        this.SetAsInteger(rtl.trunc(AValue))}
       else this.RaiseAccessError($impl.SInteger);
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      Result = this.GetAsInteger();
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt15.Create$1.call(this,AOwner);
      this.SetDataType(2);
      this.FMinRange = -2147483648;
      this.FMaxRange = 2147483647;
      return this;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.longint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.longint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass(this,"TLargeintField",$lt15,function () {
    $lt17 = this;
    this.$init = function () {
      $lt15.$init.call(this);
      this.FMinValue = 0;
      this.FMaxValue = 0;
      this.FMinRange = 0;
      this.FMaxRange = 0;
    };
    this.SetMinValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMinValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.SetMaxValue = function (AValue) {
      if ((AValue >= this.FMinRange) && (AValue <= this.FMaxRange)) {
        this.FMaxValue = AValue}
       else this.RangeError(AValue,this.FMinRange,this.FMaxRange);
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      Result = this.GetAsLargeInt();
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = this.GetAsLargeInt();
      return Result;
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      if (!this.GetValue({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }})) Result = 0;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = $lp2(L)}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var L = 0;
      if (this.GetValue({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }})) {
        Result = L}
       else Result = null;
      return Result;
    };
    this.GetValue = function (AValue) {
      var Result = false;
      var P = undefined;
      P = this.GetData();
      Result = $lp14(P);
      if (Result) AValue.set(rtl.trunc(P));
      return Result;
    };
    this.SetAsLargeInt = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.RangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetVarValue = function (AValue) {
      if ($lp14(AValue)) {
        this.SetAsLargeInt(rtl.trunc(AValue))}
       else this.RaiseAccessError($impl.SLargeInt);
    };
    this.Create$1 = function (AOwner) {
      $lt15.Create$1.call(this,AOwner);
      this.SetDataType(3);
      this.FMinRange = -9007199254740991;
      this.FMaxRange = 9007199254740991;
      return this;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = (AValue >= this.FMinRange) && (AValue <= this.FMaxRange);
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("MaxValue",2,rtl.nativeint,"FMaxValue","SetMaxValue",{Default: 0});
    $r.addProperty("MinValue",2,rtl.nativeint,"FMinValue","SetMinValue",{Default: 0});
  });
  rtl.createClass(this,"TAutoIncField",$lt16,function () {
    $lt18 = this;
    this.SetAsInteger = function (AValue) {
      $lt16.SetAsInteger.apply(this,arguments);
    };
    this.Create$1 = function (AOwner) {
      $lt16.Create$1.call(this,AOwner);
      this.SetDataType(9);
      return this;
    };
  });
  rtl.createClass(this,"TFloatField",$lt15,function () {
    $lt19 = this;
    this.$init = function () {
      $lt15.$init.call(this);
      this.FCurrency = false;
      this.FMaxValue = 0.0;
      this.FMinValue = 0.0;
      this.FPrecision = 0;
    };
    this.SetCurrency = function (AValue) {
      if (this.FCurrency === AValue) return;
      this.FCurrency = AValue;
    };
    this.SetPrecision = function (AValue) {
      if ((AValue === -1) || (AValue > 1)) {
        this.FPrecision = AValue}
       else this.FPrecision = 2;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = rtl.getNumber(P)}
       else Result = 0.0;
      return Result;
    };
    this.GetAsLargeInt = function () {
      var Result = 0;
      Result = Math.round(this.GetAsFloat());
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = Math.round(this.GetAsFloat());
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = P}
       else Result = null;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if (rtl.isNumber(P)) {
        Result = $lp15(rtl.getNumber(P))}
       else Result = "";
      return Result;
    };
    this.SetAsFloat = function (AValue) {
      if (this.CheckRange(AValue)) {
        this.SetData(AValue)}
       else this.RangeError(AValue,this.FMinValue,this.FMaxValue);
    };
    this.SetVarValue = function (AValue) {
      if (rtl.isNumber(AValue)) {
        this.SetAsFloat(rtl.getNumber(AValue))}
       else this.RaiseAccessError("Float");
    };
    this.Create$1 = function (AOwner) {
      $lt15.Create$1.call(this,AOwner);
      this.SetDataType(5);
      this.FPrecision = 15;
      return this;
    };
    this.CheckRange = function (AValue) {
      var Result = false;
      if ((this.FMinValue !== 0) || (this.FMaxValue !== 0)) {
        Result = (AValue >= this.FMinValue) && (AValue <= this.FMaxValue)}
       else Result = true;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Currency",2,rtl.boolean,"FCurrency","SetCurrency",{Default: false});
    $r.addProperty("MaxValue",0,rtl.double,"FMaxValue","FMaxValue");
    $r.addProperty("MinValue",0,rtl.double,"FMinValue","FMinValue");
    $r.addProperty("Precision",2,rtl.longint,"FPrecision","SetPrecision",{Default: 15});
  });
  rtl.createClass(this,"TBooleanField",$lt13,function () {
    $lt20 = this;
    this.FDisplays$a$clone = function (a) {
      var b = [];
      b.length = 2;
      for (var c = 0; c < 2; c++) b[c] = a[c].slice(0);
      return b;
    };
    this.$init = function () {
      $lt13.$init.call(this);
      this.FDisplayValues = "";
      this.FDisplays = rtl.arraySetLength(null,"",2,2);
    };
    this.$final = function () {
      this.FDisplays = undefined;
      $lt13.$final.call(this);
    };
    this.SetDisplayValues = function (AValue) {
      var I = 0;
      if (this.FDisplayValues !== AValue) {
        I = $lp4(";",AValue);
        if ((I < 2) || (I === AValue.length)) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidDisplayValues"),$lp(18,AValue));
        this.FDisplayValues = AValue;
        this.FDisplays[0][1] = $lp1(AValue,1,I - 1);
        this.FDisplays[1][1] = $lp6(this.FDisplays[0][1]);
        this.FDisplays[0][0] = $lp1(AValue,I + 1,AValue.length - I);
        this.FDisplays[1][0] = $lp6(this.FDisplays[0][0]);
        this.PropertyChanged(true);
      };
    };
    this.GetAsBoolean = function () {
      var Result = false;
      var P = undefined;
      P = this.GetData();
      if ($lp16(P)) {
        Result = !(P == false)}
       else Result = false;
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var P = undefined;
      P = this.GetData();
      if ($lp16(P)) {
        Result = this.FDisplays[0][+!(P == false)]}
       else Result = "";
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      var P = undefined;
      P = this.GetData();
      if ($lp16(P)) {
        Result = !(P == false)}
       else Result = null;
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      Result = this.GetAsBoolean() + 0;
      return Result;
    };
    this.GetDefaultWidth = function () {
      var Result = 0;
      Result = this.FDisplays[0][0].length;
      if (Result < this.FDisplays[0][1].length) Result = this.FDisplays[0][1].length;
      return Result;
    };
    this.SetAsBoolean = function (AValue) {
      this.SetData(AValue);
    };
    this.SetVarValue = function (AValue) {
      if ($lp16(AValue)) {
        this.SetAsBoolean(!(AValue == false))}
       else if (rtl.isNumber(AValue)) this.SetAsBoolean(rtl.getNumber(AValue) !== 0);
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      this.SetDataType(4);
      this.SetDisplayValues("True;False");
      return this;
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayValues",2,rtl.string,"FDisplayValues","SetDisplayValues");
  });
  rtl.createClass(this,"TDateTimeField",$lt13,function () {
    $lt21 = this;
    this.$init = function () {
      $lt13.$init.call(this);
      this.FDisplayFormat = "";
    };
    this.SetDisplayFormat = function (AValue) {
      if (this.FDisplayFormat !== AValue) {
        this.FDisplayFormat = AValue;
        this.PropertyChanged(true);
      };
    };
    this.ConvertToDateTime = function (aValue, aRaiseError) {
      var Result = 0.0;
      if ($lp7(aValue)) {
        Result = 0}
       else if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertToDateTime(this,aValue,aRaiseError)}
       else Result = $lt46.DefaultConvertToDateTime(this,aValue,aRaiseError);
      return Result;
    };
    this.DateTimeToNativeDateTime = function (aValue) {
      var Result = undefined;
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertDateTimeToNative(this,aValue)}
       else Result = $lt46.DefaultConvertDateTimeToNative(this,aValue);
      return Result;
    };
    this.GetAsDateTime = function () {
      var Result = 0.0;
      Result = this.ConvertToDateTime(this.GetData(),false);
      return Result;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      Result = this.GetAsDateTime();
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      this.GetText({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},false);
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      if (!rtl.isString(Result) && !rtl.isObject(Result)) Result = null;
      return Result;
    };
    this.GetText = function (AText, ADisplayText) {
      var R = 0.0;
      var F = "";
      R = this.ConvertToDateTime(this.GetData(),false);
      if (R === 0) {
        AText.set("")}
       else {
        if (ADisplayText && (this.FDisplayFormat.length !== 0)) {
          F = this.FDisplayFormat}
         else {
          var $tmp = this.FDataType;
          if ($tmp === 7) {
            F = $lm.LongTimeFormat}
           else if ($tmp === 6) {
            F = $lm.ShortDateFormat}
           else {
            F = "c";
          };
        };
        AText.set($lp17(F,R));
      };
    };
    this.SetAsDateTime = function (AValue) {
      this.SetData(this.DateTimeToNativeDateTime(AValue));
    };
    this.SetVarValue = function (AValue) {
      this.SetAsDateTime(this.ConvertToDateTime(AValue,true));
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      this.SetDataType(8);
      return this;
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayFormat",2,rtl.string,"FDisplayFormat","SetDisplayFormat");
  });
  rtl.createClass(this,"TDateField",$lt21,function () {
    $lt22 = this;
    this.Create$1 = function (AOwner) {
      $lt21.Create$1.call(this,AOwner);
      this.SetDataType(6);
      return this;
    };
  });
  rtl.createClass(this,"TTimeField",$lt21,function () {
    $lt23 = this;
    this.Create$1 = function (AOwner) {
      $lt21.Create$1.call(this,AOwner);
      this.SetDataType(7);
      return this;
    };
  });
  rtl.createClass(this,"TBinaryField",$lt13,function () {
    $lt24 = this;
    this.CheckTypeSize = function (AValue) {
      if (AValue < 1) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidFieldSize"),$lp(0,AValue));
    };
    this.BlobToBytes = function (aValue) {
      var Result = [];
      if (this.FDataSet != null) {
        Result = this.FDataSet.BlobDataToBytes(aValue)}
       else Result = $lt46.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.BytesToBlob = function (aValue) {
      var Result = undefined;
      if (this.FDataSet != null) {
        Result = this.FDataSet.BytesToBlobData(rtl.arrayRef(aValue))}
       else Result = $lt46.DefaultBytesToBlobData(rtl.arrayRef(aValue));
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      var S = [];
      var I = 0;
      Result = "";
      V = this.GetData();
      if (V != null) if (this.FDataType === 11) {
        Result = "" + V}
       else {
        S = this.BlobToBytes(V);
        for (var $l = 0, $end = rtl.length(S) - 1; $l <= $end; $l++) {
          I = $l;
          Result = Result.concat(String.fromCharCode(S[I]));
        };
      };
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetAsString = function (AValue) {
      var B = [];
      var i = 0;
      if (this.FDataType === 11) {
        this.SetData(AValue)}
       else {
        B = rtl.arraySetLength(B,0,AValue.length);
        for (var $l = 1, $end = AValue.length; $l <= $end; $l++) {
          i = $l;
          B[i - 1] = AValue.charCodeAt(i - 1);
        };
        this.SetAsBytes(B);
      };
    };
    this.SetVarValue = function (AValue) {
      var B = [];
      var I = 0;
      var Len = 0;
      if (rtl.isArray(AValue)) {
        Len = rtl.length(AValue);
        B = rtl.arraySetLength(B,0,Len);
        for (var $l = 1, $end = Len - 1; $l <= $end; $l++) {
          I = $l;
          B[I] = AValue[I];
        };
        this.SetAsBytes(B);
      } else if (rtl.isString(AValue)) {
        this.SetAsString("" + AValue)}
       else this.RaiseAccessError("Blob");
    };
    this.GetAsBytes = function () {
      var Result = [];
      var V = undefined;
      V = this.GetData();
      if ($lp18(V)) {
        Result = this.BlobToBytes(V)}
       else Result = rtl.arraySetLength(Result,0,0);
      return Result;
    };
    this.SetAsBytes = function (aValue) {
      this.SetData(this.BytesToBlob(rtl.arrayRef(aValue)));
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      return this;
    };
    var $r = this.$rtti;
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 16});
  });
  $lt25 = this.TBlobDisplayValue = {"0": "dvClass", dvClass: 0, "1": "dvFull", dvFull: 1, "2": "dvClip", dvClip: 2, "3": "dvFit", dvFit: 3};
  this.$rtti.$Enum("TBlobDisplayValue",{minvalue: 0, maxvalue: 3, ordtype: 1, enumtype: this.TBlobDisplayValue});
  this.$rtti.$Enum("TBlobType",{minvalue: 10, maxvalue: 11, ordtype: 1, enumtype: this.TFieldType});
  rtl.createClass(this,"TBlobField",$lt24,function () {
    $lt26 = this;
    this.$init = function () {
      $lt24.$init.call(this);
      this.FDisplayValue = 0;
      this.FModified = false;
    };
    this.GetBlobType = function () {
      var Result = $lt5.ftBlob;
      Result = 10;
      return Result;
    };
    this.SetBlobType = function (AValue) {
      this.SetFieldType(AValue);
    };
    this.SetDisplayValue = function (AValue) {
      if (this.FDisplayValue === AValue) return;
      this.FDisplayValue = AValue;
      this.PropertyChanged(false);
    };
    this.CheckTypeSize = function (AValue) {
      if (AValue < 0) $mod.DatabaseErrorFmt(rtl.getResStr($lm2,"SInvalidFieldSize"),$lp(0,AValue));
    };
    this.GetBlobSize = function () {
      var Result = 0;
      var B = [];
      B = this.GetAsBytes();
      Result = rtl.length(B);
      return Result;
    };
    this.GetIsNull = function () {
      var Result = false;
      if (!this.FModified) {
        Result = $lt13.GetIsNull.call(this)}
       else Result = this.GetBlobSize() === 0;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt24.Create$1.call(this,AOwner);
      this.SetDataType(10);
      return this;
    };
    this.Clear = function () {
      this.SetData(null);
    };
    this.IsBlob = function () {
      var Result = false;
      Result = true;
      return Result;
    };
    this.SetFieldType = function (AValue) {
      if (AValue in $mod.ftBlobTypes) this.SetDataType(AValue);
    };
    var $r = this.$rtti;
    $r.addProperty("DisplayValue",2,$mod.$rtti["TBlobDisplayValue"],"FDisplayValue","SetDisplayValue",{Default: $lt25.dvClass});
    $r.addProperty("BlobType",3,$mod.$rtti["TBlobType"],"GetBlobType","SetBlobType");
    $r.addProperty("Size",2,rtl.longint,"FSize","SetSize",{Default: 0});
  });
  rtl.createClass(this,"TMemoField",$lt26,function () {
    $lt27 = this;
    this.Create$1 = function (AOwner) {
      $lt26.Create$1.call(this,AOwner);
      this.SetDataType(11);
      return this;
    };
  });
  rtl.createClass(this,"TVariantField",$lt13,function () {
    $lt28 = this;
    this.CheckTypeSize = function (aValue) {
    };
    this.GetAsBoolean = function () {
      var Result = false;
      Result = this.GetAsJSValue() == true;
      return Result;
    };
    this.GetAsDateTime = function () {
      var Result = 0.0;
      var V = undefined;
      V = this.GetData();
      if (this.FDataSet != null) {
        Result = this.FDataSet.ConvertToDateTime(this,V,true)}
       else Result = $lt46.DefaultConvertToDateTime(this,V,true);
      return Result;
    };
    this.GetAsFloat = function () {
      var Result = 0.0;
      var V = undefined;
      V = this.GetData();
      if (rtl.isNumber(V)) {
        Result = rtl.getNumber(V)}
       else if (rtl.isString(V)) {
        Result = parseFloat("" + V)}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.GetAsInteger = function () {
      var Result = 0;
      var V = undefined;
      V = this.GetData();
      if ($lp14(V)) {
        Result = rtl.trunc(V)}
       else if (rtl.isString(V)) {
        Result = parseInt("" + V)}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.GetAsString = function () {
      var Result = "";
      var V = undefined;
      V = this.GetData();
      if ($lp14(V)) {
        Result = $lp2(rtl.trunc(V))}
       else if (rtl.isNumber(V)) {
        Result = $lp15(rtl.getNumber(V))}
       else if (rtl.isString(V)) {
        Result = "" + V}
       else this.RaiseAccessError("Variant");
      return Result;
    };
    this.GetAsJSValue = function () {
      var Result = undefined;
      Result = this.GetData();
      return Result;
    };
    this.SetVarValue = function (aValue) {
      this.SetData(aValue);
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.call(this,AOwner);
      this.SetDataType(13);
      return this;
    };
  });
  rtl.createClass(this,"TDataSetField",$lt13,function () {
    $lt29 = this;
    this.$init = function () {
      $lt13.$init.call(this);
      this.FNestedDataSet = null;
    };
    this.$final = function () {
      this.FNestedDataSet = undefined;
      $lt13.$final.call(this);
    };
    this.AssignNestedDataSet = function (Value) {
      if (this.FNestedDataSet != null) {
        this.FNestedDataSet.Close();
        this.FNestedDataSet.FDataSetField = null;
        if (this.FDataSet != null) this.FDataSet.GetNestedDataSets().Remove(this.FNestedDataSet);
      };
      if (Value != null) this.FDataSet.GetNestedDataSets().Add(Value);
      this.FNestedDataSet = Value;
    };
    this.Bind = function (Binding) {
      $lt13.Bind.apply(this,arguments);
      if (this.FNestedDataSet != null) if (Binding) {
        if (this.FNestedDataSet.FState === 0) this.FNestedDataSet.Open();
      } else this.FNestedDataSet.Close();
    };
    this.Create$1 = function (AOwner) {
      $lt13.Create$1.apply(this,arguments);
      this.SetDataType(14);
      return this;
    };
    this.Destroy = function () {
      this.AssignNestedDataSet(null);
      $lt13.Destroy.call(this);
    };
  });
  $lt30 = this.TIndexOption = {"0": "ixPrimary", ixPrimary: 0, "1": "ixUnique", ixUnique: 1, "2": "ixDescending", ixDescending: 2, "3": "ixCaseInsensitive", ixCaseInsensitive: 3, "4": "ixExpression", ixExpression: 4, "5": "ixNonMaintained", ixNonMaintained: 5};
  this.$rtti.$Enum("TIndexOption",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TIndexOption});
  this.$rtti.$Set("TIndexOptions",{comptype: this.$rtti["TIndexOption"]});
  rtl.createClass(this,"TIndexDef",$lt7,function () {
    $lt31 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FCaseinsFields = "";
      this.FDescFields = "";
      this.FExpression = "";
      this.FFields = "";
      this.FOptions = {};
      this.FSource = "";
    };
    this.$final = function () {
      this.FOptions = undefined;
      $lt7.$final.call(this);
    };
    this.GetExpression = function () {
      var Result = "";
      Result = this.FExpression;
      return Result;
    };
    this.SetCaseInsFields = function (AValue) {
      if (this.FCaseinsFields === AValue) return;
      if (AValue !== "") this.FOptions = rtl.unionSet(this.FOptions,rtl.createSet(3));
      this.FCaseinsFields = AValue;
    };
    this.SetDescFields = function (AValue) {
      if (this.FDescFields === AValue) return;
      if (AValue !== "") this.FOptions = rtl.unionSet(this.FOptions,rtl.createSet(2));
      this.FDescFields = AValue;
    };
    this.SetExpression = function (AValue) {
      this.FExpression = AValue;
    };
    this.Assign = function (Source) {
      var idef = null;
      idef = null;
      if ($lt31.isPrototypeOf(Source)) idef = rtl.as(Source,$lt31);
      if (idef != null) {
        this.FName = idef.FName;
        this.FFields = idef.FFields;
        this.FOptions = rtl.refSet(idef.FOptions);
        this.FCaseinsFields = idef.FCaseinsFields;
        this.FDescFields = idef.FDescFields;
        this.FSource = idef.FSource;
        this.FExpression = idef.GetExpression();
      } else $lt57.Assign.call(this,Source);
    };
    var $r = this.$rtti;
    $r.addProperty("Expression",3,rtl.string,"GetExpression","SetExpression");
    $r.addProperty("Fields",0,rtl.string,"FFields","FFields");
    $r.addProperty("CaseInsFields",2,rtl.string,"FCaseinsFields","SetCaseInsFields");
    $r.addProperty("DescFields",2,rtl.string,"FDescFields","SetDescFields");
    $r.addProperty("Options",0,$mod.$rtti["TIndexOptions"],"FOptions","FOptions");
    $r.addProperty("Source",0,rtl.string,"FSource","FSource");
  });
  rtl.createClass(this,"TIndexDefs",$lt8,function () {
    $lt32 = this;
    this.GetItem$1 = function (Index) {
      var Result = null;
      Result = rtl.as($lt56.GetItem.call(this,Index),$lt31);
      return Result;
    };
    this.Find$1 = function (IndexName) {
      var Result = null;
      Result = rtl.as($lt8.Find.call(this,IndexName),$lt31);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SIndexNotFound"),$lp(18,IndexName),this.FDataset);
      return Result;
    };
  });
  rtl.createClass(this,"TCheckConstraint",$lt54,function () {
    $lt33 = this;
    this.$init = function () {
      $lt54.$init.call(this);
      this.FCustomConstraint = "";
      this.FErrorMessage = "";
      this.FFromDictionary = false;
      this.FImportedConstraint = "";
    };
    this.Assign = function (Source) {
    };
    var $r = this.$rtti;
    $r.addProperty("CustomConstraint",0,rtl.string,"FCustomConstraint","FCustomConstraint");
    $r.addProperty("ErrorMessage",0,rtl.string,"FErrorMessage","FErrorMessage");
    $r.addProperty("FromDictionary",0,rtl.boolean,"FFromDictionary","FFromDictionary");
    $r.addProperty("ImportedConstraint",0,rtl.string,"FImportedConstraint","FImportedConstraint");
  });
  rtl.createClass(this,"TCheckConstraints",$lt56,function () {
    $lt34 = this;
    this.GetOwner = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.Create$2 = function (AOwner) {
      $lt56.Create$1.call(this,$lt33);
      return this;
    };
  });
  rtl.createClass(this,"TFields",$lt58,function () {
    $lt35 = this;
    this.$init = function () {
      $lt58.$init.call(this);
      this.FDataset = null;
      this.FFieldList = null;
      this.FOnChange = null;
      this.FValidFieldKinds = {};
    };
    this.$final = function () {
      this.FDataset = undefined;
      this.FFieldList = undefined;
      this.FOnChange = undefined;
      this.FValidFieldKinds = undefined;
      $lt58.$final.call(this);
    };
    this.ClearFieldDefs = function () {
      var i = 0;
      for (var $l = 0, $end = this.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        this.GetField(i).FFieldDef = null;
      };
    };
    this.Changed = function () {
      if ((this.FDataset !== null) && !(3 in this.FDataset.FComponentState)) this.FDataset.DataEvent(9,0);
      if (this.FOnChange != null) this.FOnChange(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FFieldList.FCount;
      return Result;
    };
    this.GetField = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FFieldList.Get(Index));
      return Result;
    };
    this.SetFieldIndex = function (Field, Value) {
      var Old = 0;
      Old = this.FFieldList.IndexOf(Field);
      if (Old === -1) return;
      if (Value < 0) Value = 0;
      if (Value >= this.GetCount()) Value = this.GetCount() - 1;
      if (Value !== Old) {
        this.FFieldList.Delete(Old);
        this.FFieldList.Insert(Value,Field);
        Field.PropertyChanged(true);
        this.Changed();
      };
    };
    this.Create$1 = function (ADataset) {
      this.FDataset = ADataset;
      this.FFieldList = $lt59.$create("Create");
      this.FValidFieldKinds = rtl.createSet(null,0,3);
      return this;
    };
    this.Destroy = function () {
      if (this.FFieldList != null) this.Clear();
      $lp19({p: this, get: function () {
          return this.p.FFieldList;
        }, set: function (v) {
          this.p.FFieldList = v;
        }});
      $lt58.Destroy.call(this);
    };
    this.Add = function (Field) {
      this.CheckFieldName(Field.FFieldName);
      this.FFieldList.Add(Field);
      Field.FFields = this;
      this.Changed();
    };
    this.CheckFieldName = function (Value) {
      if (this.FindField(Value) !== null) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SDuplicateFieldName"),$lp(18,Value),this.FDataset);
    };
    this.CheckFieldNames = function (Value) {
      var N = "";
      var StrPos = 0;
      if (Value === "") return;
      StrPos = 1;
      do {
        N = $mod.ExtractFieldName(Value,{get: function () {
            return StrPos;
          }, set: function (v) {
            StrPos = v;
          }});
        this.FieldByName(N);
      } while (!(StrPos > Value.length));
    };
    this.Clear = function () {
      var AField = null;
      while (this.FFieldList.FCount > 0) {
        AField = rtl.getObject(this.FFieldList.Last());
        AField.FDataSet = null;
        AField = rtl.freeLoc(AField);
        this.FFieldList.Delete(this.FFieldList.FCount - 1);
      };
      this.Changed();
    };
    this.FindField = function (Value) {
      var Result = null;
      var S = "";
      var I = 0;
      S = $lp6(Value);
      for (var $l = 0, $end = this.FFieldList.FCount - 1; $l <= $end; $l++) {
        I = $l;
        Result = rtl.getObject(this.FFieldList.Get(I));
        if (S === $lp6(Result.FFieldName)) {
          return Result;
        };
      };
      Result = null;
      return Result;
    };
    this.FieldByName = function (Value) {
      var Result = null;
      Result = this.FindField(Value);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SFieldNotFound"),$lp(18,Value),this.FDataset);
      return Result;
    };
    this.IndexOf = function (Field) {
      var Result = 0;
      Result = this.FFieldList.IndexOf(Field);
      return Result;
    };
    this.Remove = function (Value) {
      this.FFieldList.Remove(Value);
      Value.FFields = null;
      this.Changed();
    };
  });
  $lt36 = this.TBookmarkFlag = {"0": "bfCurrent", bfCurrent: 0, "1": "bfBOF", bfBOF: 1, "2": "bfEOF", bfEOF: 2, "3": "bfInserted", bfInserted: 3};
  rtl.recNewT(this,"TBookmark",function () {
    $lt37 = this;
    this.Data = undefined;
    this.$eq = function (b) {
      return this.Data === b.Data;
    };
    this.$assign = function (s) {
      this.Data = s.Data;
      return this;
    };
  });
  $lt38 = this.TGetMode = {"0": "gmCurrent", gmCurrent: 0, "1": "gmNext", gmNext: 1, "2": "gmPrior", gmPrior: 2};
  $lt39 = this.TGetResult = {"0": "grOK", grOK: 0, "1": "grBOF", grBOF: 1, "2": "grEOF", grEOF: 2, "3": "grError", grError: 3};
  $lt40 = this.TResyncMode$a = {"0": "rmExact", rmExact: 0, "1": "rmCenter", rmCenter: 1};
  $lt41 = this.TDataAction = {"0": "daFail", daFail: 0, "1": "daAbort", daAbort: 1, "2": "daRetry", daRetry: 2};
  this.$rtti.$Enum("TDataAction",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TDataAction});
  $lt42 = this.TLocateOption = {"0": "loCaseInsensitive", loCaseInsensitive: 0, "1": "loPartialKey", loPartialKey: 1, "2": "loFromCurrent", loFromCurrent: 2};
  this.$rtti.$MethodVar("TDataSetNotifyEvent",{procsig: rtl.newTIProcSig([["DataSet",this.$rtti["TDataSet"]]]), methodkind: 0});
  this.$rtti.$MethodVar("TDataSetErrorEvent",{procsig: rtl.newTIProcSig([["DataSet",this.$rtti["TDataSet"]],["E",this.$rtti["EDatabaseError"]],["DataAction",this.$rtti["TDataAction"],1]]), methodkind: 0});
  $lt43 = this.TLoadOption = {"0": "loNoOpen", loNoOpen: 0, "1": "loNoEvents", loNoEvents: 1, "2": "loAtEOF", loAtEOF: 2, "3": "loCancelPending", loCancelPending: 3};
  this.$rtti.$MethodVar("TFilterRecordEvent",{procsig: rtl.newTIProcSig([["DataSet",this.$rtti["TDataSet"]],["Accept",rtl.boolean,1]]), methodkind: 0});
  $lt44 = this.TRecordState = {"0": "rsNew", rsNew: 0, "1": "rsClean", rsClean: 1, "2": "rsUpdate", rsUpdate: 2, "3": "rsDelete", rsDelete: 3};
  rtl.recNewT(this,"TDataRecord",function () {
    $lt45 = this;
    this.data = undefined;
    this.state = 0;
    this.bookmark = undefined;
    this.bookmarkFlag = 0;
    this.$eq = function (b) {
      return (this.data === b.data) && (this.state === b.state) && (this.bookmark === b.bookmark) && (this.bookmarkFlag === b.bookmarkFlag);
    };
    this.$assign = function (s) {
      this.data = s.data;
      this.state = s.state;
      this.bookmark = s.bookmark;
      this.bookmarkFlag = s.bookmarkFlag;
      return this;
    };
  });
  rtl.createClass(this,"TDataSet",$lt60,function () {
    $lt46 = this;
    this.$init = function () {
      $lt60.$init.call(this);
      this.FAfterLoad = null;
      this.FBeforeLoad = null;
      this.FBlockReadSize = 0;
      this.FCalcBuffer = $lt45.$new();
      this.FCalcFieldsCount = 0;
      this.FOnLoadFail = null;
      this.FOpenAfterRead = false;
      this.FActiveRecord = 0;
      this.FAfterCancel = null;
      this.FAfterClose = null;
      this.FAfterDelete = null;
      this.FAfterEdit = null;
      this.FAfterInsert = null;
      this.FAfterOpen = null;
      this.FAfterPost = null;
      this.FAfterScroll = null;
      this.FAutoCalcFields = false;
      this.FBOF = false;
      this.FBeforeCancel = null;
      this.FBeforeClose = null;
      this.FBeforeDelete = null;
      this.FBeforeEdit = null;
      this.FBeforeInsert = null;
      this.FBeforeOpen = null;
      this.FBeforePost = null;
      this.FBeforeScroll = null;
      this.FBlobFieldCount = 0;
      this.FBuffers = [];
      this.FBufferCount = 0;
      this.FConstraints = null;
      this.FDisableControlsCount = 0;
      this.FDisableControlsState = 0;
      this.FCurrentRecord = 0;
      this.FDataSources = null;
      this.FDefaultFields = false;
      this.FEOF = false;
      this.FEnableControlsEvent = 0;
      this.FFieldList = null;
      this.FFieldDefs = null;
      this.FFilterText = "";
      this.FFiltered = false;
      this.FInternalCalcFields = false;
      this.FModified = false;
      this.FOnCalcFields = null;
      this.FOnDeleteError = null;
      this.FOnEditError = null;
      this.FOnFilterRecord = null;
      this.FOnNewRecord = null;
      this.FOnPostError = null;
      this.FRecordCount = 0;
      this.FIsUniDirectional = false;
      this.FState = 0;
      this.FInternalOpenComplete = false;
      this.FDataProxy = null;
      this.FDataRequestID = 0;
      this.FChangeList = null;
      this.FLoadCount = 0;
      this.FMinLoadID = 0;
      this.FDataSetField = null;
      this.FNestedDataSets = null;
      this.FNestedDataSetClass = null;
    };
    this.$final = function () {
      this.FAfterLoad = undefined;
      this.FBeforeLoad = undefined;
      this.FCalcBuffer = undefined;
      this.FOnLoadFail = undefined;
      this.FAfterCancel = undefined;
      this.FAfterClose = undefined;
      this.FAfterDelete = undefined;
      this.FAfterEdit = undefined;
      this.FAfterInsert = undefined;
      this.FAfterOpen = undefined;
      this.FAfterPost = undefined;
      this.FAfterScroll = undefined;
      this.FBeforeCancel = undefined;
      this.FBeforeClose = undefined;
      this.FBeforeDelete = undefined;
      this.FBeforeEdit = undefined;
      this.FBeforeInsert = undefined;
      this.FBeforeOpen = undefined;
      this.FBeforePost = undefined;
      this.FBeforeScroll = undefined;
      this.FBuffers = undefined;
      this.FConstraints = undefined;
      this.FDataSources = undefined;
      this.FFieldList = undefined;
      this.FFieldDefs = undefined;
      this.FOnCalcFields = undefined;
      this.FOnDeleteError = undefined;
      this.FOnEditError = undefined;
      this.FOnFilterRecord = undefined;
      this.FOnNewRecord = undefined;
      this.FOnPostError = undefined;
      this.FDataProxy = undefined;
      this.FChangeList = undefined;
      this.FDataSetField = undefined;
      this.FNestedDataSets = undefined;
      this.FNestedDataSetClass = undefined;
      $lt60.$final.call(this);
    };
    this.DoInternalOpen = function () {
      this.InternalOpen();
      this.FInternalOpenComplete = true;
      this.FRecordCount = 0;
      this.RecalcBufListSize();
      this.FBOF = true;
      this.FEOF = this.FRecordCount === 0;
      if (this.GetDataProxy() != null) this.InitChangeList();
    };
    this.GetDataProxy = function () {
      var Result = null;
      if (this.FDataProxy === null) this.SetDataProxy(this.DoGetDataProxy());
      Result = this.FDataProxy;
      return Result;
    };
    this.RegisterDataSource = function (ADataSource) {
      this.FDataSources.Add(ADataSource);
      this.RecalcBufListSize();
    };
    this.SetDataProxy = function (AValue) {
      if (AValue === this.FDataProxy) return;
      if (this.FDataProxy != null) this.FDataProxy.RemoveFreeNotification(this);
      this.FDataProxy = AValue;
      if (this.FDataProxy != null) this.FDataProxy.FreeNotification(this);
    };
    this.ShiftBuffersForward = function () {
      var TempBuf = $lt45.$new();
      var I = 0;
      TempBuf.$assign(this.FBuffers[this.FBufferCount]);
      for (var $l = this.FBufferCount; $l >= 1; $l--) {
        I = $l;
        this.FBuffers[I].$assign(this.FBuffers[I - 1]);
      };
      this.FBuffers[0].$assign(TempBuf);
    };
    this.ShiftBuffersBackward = function () {
      var TempBuf = $lt45.$new();
      var I = 0;
      TempBuf.$assign(this.FBuffers[0]);
      for (var $l = 1, $end = this.FBufferCount; $l <= $end; $l++) {
        I = $l;
        this.FBuffers[I - 1].$assign(this.FBuffers[I]);
      };
      this.FBuffers[this.FBufferCount].$assign(TempBuf);
    };
    this.TryDoing = function (P, Ev) {
      var Result = false;
      var Retry = 0;
      Result = true;
      Retry = 2;
      while (Retry === 2) try {
        this.UpdateCursorPos();
        P();
        return Result;
      } catch ($e) {
        if ($lt4.isPrototypeOf($e)) {
          var E = $e;
          Retry = 0;
          if (Ev != null) Ev(this,E,{get: function () {
              return Retry;
            }, set: function (v) {
              Retry = v;
            }});
          var $tmp = Retry;
          if ($tmp === 0) {
            throw $e}
           else if ($tmp === 1) $lp20();
        } else {
          throw $e;
        }
      };
      return Result;
    };
    this.GetActive = function () {
      var Result = false;
      Result = (this.FState !== 0) && (this.FState !== 12);
      return Result;
    };
    this.UnRegisterDataSource = function (ADataSource) {
      this.FDataSources.Remove(ADataSource);
    };
    this.SetFieldDefs = function (AFieldDefs) {
      this.FFieldList.ClearFieldDefs();
      this.FFieldDefs.Assign$2(AFieldDefs);
    };
    this.HandleRequestResponse = function (ARequest) {
      var DataAdded = false;
      if (!(ARequest != null)) return;
      if (ARequest.FRequestID <= this.FMinLoadID) {
        ARequest.$destroy("Destroy");
        return;
      };
      this.FLoadCount -= 1;
      var $tmp = ARequest.FSuccess;
      if ($tmp === 0) {
        if (this.FOnLoadFail != null) this.FOnLoadFail(this,ARequest.FRequestID,ARequest.FErrorMsg);
      } else if (($tmp === 1) || ($tmp === 2)) {
        DataAdded = false;
        if (ARequest.FEvent != null) ARequest.FEvent(this,ARequest.FData);
        if (ARequest.FSuccess !== 1) DataAdded = this.DataPacketReceived(ARequest);
        if (!(this.GetActive() || (0 in ARequest.FLoadOptions))) {
          if (!(1 in ARequest.FLoadOptions)) this.DoAfterLoad();
          this.Open();
        } else {
          if ((2 in ARequest.FLoadOptions) && DataAdded) this.FEOF = false;
          if (!(1 in ARequest.FLoadOptions)) this.DoAfterLoad();
        };
      };
      ARequest.$destroy("Destroy");
    };
    this.GetNestedDataSets = function () {
      var Result = null;
      if (!(this.FNestedDataSets != null)) this.FNestedDataSets = $lt59.$create("Create");
      Result = this.FNestedDataSets;
      return Result;
    };
    this.DataPacketReceived = function (ARequest) {
      var Result = false;
      Result = false;
      return Result;
    };
    this.DoLoad = function (aOptions, aAfterLoad) {
      var Result = false;
      var Request = null;
      if (!(1 in aOptions)) this.DoBeforeLoad();
      Result = this.GetDataProxy() !== null;
      if (!Result) return Result;
      Request = this.GetDataProxy().GetDataRequest(rtl.refSet(aOptions),rtl.createCallback(this,"HandleRequestResponse"),aAfterLoad);
      Request.FDataset = this;
      if (this.GetActive()) Request.FBookmark.$assign(this.GetBookmark());
      this.FDataRequestID += 1;
      Request.FRequestID = this.FDataRequestID;
      if (this.GetDataProxy().DoGetData(Request)) {
        this.FLoadCount += 1}
       else Request = rtl.freeLoc(Request);
      return Result;
    };
    this.DoGetDataProxy = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InitChangeList = function () {
      this.DoneChangeList();
      this.FChangeList = $lt59.$create("Create");
    };
    this.DoneChangeList = function () {
      this.ClearChangeList();
      $lp19({p: this, get: function () {
          return this.p.FChangeList;
        }, set: function (v) {
          this.p.FChangeList = v;
        }});
    };
    this.ClearChangeList = function () {
      var I = 0;
      if (!(this.FChangeList != null)) return;
      for (var $l = 0, $end = this.FChangeList.FCount - 1; $l <= $end; $l++) {
        I = $l;
        rtl.getObject(this.FChangeList.Get(I)).$destroy("Destroy");
        this.FChangeList.Put(I,null);
      };
    };
    this.GetApplyUpdateData = function (aBuffer) {
      var Result = undefined;
      Result = aBuffer.data;
      return Result;
    };
    this.IndexInChangeList = function (aBookmark) {
      var Result = 0;
      Result = -1;
      if (!(this.FChangeList != null)) return Result;
      Result = this.FChangeList.FCount - 1;
      while ((Result >= 0) && (this.CompareBookmarks($lt37.$clone(aBookmark),$lt37.$clone(rtl.getObject(this.FChangeList.Get(Result)).FBookmark)) !== 0)) Result -= 1;
      return Result;
    };
    this.AddToChangeList = function (aChange) {
      var Result = null;
      var B = $lt37.$new();
      var I = 0;
      var aData = undefined;
      Result = null;
      if (!(this.FChangeList != null)) return Result;
      B.$assign(this.GetBookmark());
      I = this.IndexInChangeList($lt37.$clone(B));
      if (I === -1) {
        aData = this.GetApplyUpdateData($lt45.$clone(this.ActiveBuffer()));
        if (this.GetDataProxy() != null) {
          Result = this.GetDataProxy().GetUpdateDescriptor(this,$lt37.$clone(B),aData,aChange)}
         else Result = $lt51.$create("Create$1",[null,this,$lt37.$clone(B),aData,aChange]);
        this.FChangeList.Add(Result);
      } else {
        Result = rtl.getObject(this.FChangeList.Get(I));
        var $tmp = aChange;
        if ($tmp === 2) {
          if (Result.FStatus === 1) {
            this.FChangeList.Delete(I)}
           else Result.FStatus = 2;
        } else if ($tmp === 1) {
          $mod.DatabaseError$1(rtl.getResStr($lm2,"SErrInsertingSameRecordtwice"),this)}
         else if ($tmp === 0) Result.FData = this.GetApplyUpdateData($lt45.$clone(this.ActiveBuffer()));
      };
      return Result;
    };
    this.RecalcBufListSize = function () {
      var i = 0;
      var j = 0;
      var ABufferCount = 0;
      var DataLink = null;
      if (!this.IsCursorOpen()) return;
      if (this.FIsUniDirectional) {
        ABufferCount = 1}
       else ABufferCount = 10;
      for (var $l = 0, $end = this.FDataSources.FCount - 1; $l <= $end; $l++) {
        i = $l;
        for (var $l1 = 0, $end1 = rtl.getObject(this.FDataSources.Get(i)).FDataLinks.GetCount() - 1; $l1 <= $end1; $l1++) {
          j = $l1;
          DataLink = rtl.getObject(rtl.getObject(this.FDataSources.Get(i)).FDataLinks.Get(j));
          if (ABufferCount < DataLink.GetBufferCount()) ABufferCount = DataLink.GetBufferCount();
        };
      };
      if (this.FBufferCount === ABufferCount) return;
      this.SetBufListSize(ABufferCount);
      this.GetNextRecords();
      if ((this.FRecordCount < this.FBufferCount) && !this.FIsUniDirectional) {
        this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
        this.CursorPosChanged();
      };
    };
    this.ActivateBuffers = function () {
      this.FBOF = false;
      this.FEOF = false;
      this.FActiveRecord = 0;
    };
    this.BindFields = function (Binding) {
      var i = 0;
      var FieldIndex = 0;
      var FieldDef = null;
      var Field = null;
      this.FCalcFieldsCount = 0;
      this.FBlobFieldCount = 0;
      for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        Field = this.FFieldList.GetField(i);
        Field.FFieldDef = null;
        if (!Binding) {
          Field.FFieldNo = 0}
         else if (Field.FFieldKind in rtl.createSet(1,2)) {
          Field.FFieldNo = -1;
          this.FCalcFieldsCount += 1;
        } else {
          FieldIndex = this.FFieldDefs.IndexOf(Field.FFieldName);
          if (FieldIndex === -1) {
            $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SFieldNotFound"),$lp(18,Field.FFieldName),this)}
           else {
            FieldDef = this.FFieldDefs.GetItem$1(FieldIndex);
            Field.FFieldDef = FieldDef;
            Field.FFieldNo = FieldDef.FFieldNo;
            if (FieldDef.FInternalCalcField) this.FInternalCalcFields = true;
            if (Field.$class.IsBlob()) {
              Field.FSize = FieldDef.FSize;
              this.FBlobFieldCount += 1;
            };
          };
        };
        Field.Bind(Binding);
      };
    };
    this.BlockReadNext = function () {
      this.MoveBy(1);
    };
    var BookmarkStates = rtl.createSet(1,2,3);
    this.BookmarkAvailable = function () {
      var Result = false;
      Result = !this.IsEmpty() && !this.FIsUniDirectional && (this.FState in BookmarkStates) && (this.GetBookmarkFlag($lt45.$clone(this.ActiveBuffer())) === 0);
      return Result;
    };
    this.CalculateFields = function (Buffer) {
      var i = 0;
      this.FCalcBuffer.$assign(Buffer);
      if (this.FState !== 11) {
        this.ClearCalcFields(this.FCalcBuffer);
        if (!this.FIsUniDirectional) for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
          i = $l;
          if (this.FFieldList.GetField(i).FFieldKind === 2) this.FFieldList.GetField(i).CalcLookupValue();
        };
      };
      this.DoOnCalcFields();
    };
    this.CheckActive = function () {
      if (!this.GetActive()) $mod.DatabaseError$1(rtl.getResStr($lm2,"SInactiveDataset"),this);
    };
    this.CheckInactive = function () {
      if (this.GetActive()) $mod.DatabaseError$1(rtl.getResStr($lm2,"SActiveDataset"),this);
    };
    this.CheckBiDirectional = function () {
      if (this.FIsUniDirectional) $mod.DatabaseError$1(rtl.getResStr($lm2,"SUniDirectional"),this);
    };
    this.ClearBuffers = function () {
      this.FRecordCount = 0;
      this.FActiveRecord = 0;
      this.FCurrentRecord = -1;
      this.FBOF = true;
      this.FEOF = true;
    };
    this.ClearCalcFields = function (Buffer) {
    };
    this.CloseCursor = function () {
      this.ClearBuffers();
      this.SetBufListSize(0);
      this.FFieldList.ClearFieldDefs();
      this.InternalClose();
      this.FInternalOpenComplete = false;
    };
    this.CreateFields = function () {
      var I = 0;
      for (var $l = 0, $end = this.FFieldDefs.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        var $with = this.FFieldDefs.GetItem$1(I);
        if ($with.FDataType !== 0) {
          $with.CreateField(this);
        };
      };
    };
    this.DataEvent = function (Event, Info) {
      var $Self = this;
      function HandleFieldChange(aField) {
        if (aField.FFieldKind in rtl.createSet(0,3)) $Self.SetModified(true);
        if ($Self.FState !== 4) {
          if (aField.FFieldKind === 0) {
            if ($Self.FInternalCalcFields) {
              $Self.RefreshInternalCalcFields($Self.FBuffers[$Self.FActiveRecord])}
             else if ($Self.FAutoCalcFields && ($Self.FCalcFieldsCount !== 0)) $Self.CalculateFields($Self.FBuffers[$Self.FActiveRecord]);
          };
          aField.Change();
        };
      };
      function HandleScrollOrChange() {
        var A = 0;
        var NestedDataSet = null;
        if ($Self.FState !== 3) $Self.UpdateCursorPos();
        if ($Self.FNestedDataSets != null) for (var $l = 0, $end = $Self.GetNestedDataSets().FCount - 1; $l <= $end; $l++) {
          A = $l;
          NestedDataSet = rtl.getObject($Self.GetNestedDataSets().Get(A));
          if (NestedDataSet.GetActive()) NestedDataSet.DataEvent(11,0);
        };
      };
      var i = 0;
      var $tmp = Event;
      if ($tmp === 0) {
        HandleFieldChange(rtl.getObject(Info))}
       else if (($tmp === 2) || ($tmp === 3)) {
        HandleScrollOrChange()}
       else if ($tmp === 4) this.FEnableControlsEvent = 4;
      if (!this.ControlsDisabled() && (this.FState !== 10)) {
        for (var $l = 0, $end = this.FDataSources.FCount - 1; $l <= $end; $l++) {
          i = $l;
          rtl.getObject(this.FDataSources.Get(i)).ProcessEvent(Event,Info);
        };
      };
    };
    this.DestroyFields = function () {
      this.FFieldList.Clear();
    };
    this.DoAfterCancel = function () {
      if (this.FAfterCancel != null) this.FAfterCancel(this);
    };
    this.DoAfterClose = function () {
      if ((this.FAfterClose != null) && !(3 in this.FComponentState)) this.FAfterClose(this);
    };
    this.DoAfterOpen = function () {
      if (this.FAfterOpen != null) this.FAfterOpen(this);
    };
    this.DoAfterPost = function () {
      if (this.FAfterPost != null) this.FAfterPost(this);
    };
    this.DoAfterScroll = function () {
      if (this.FAfterScroll != null) this.FAfterScroll(this);
    };
    this.DoBeforeCancel = function () {
      if (this.FBeforeCancel != null) this.FBeforeCancel(this);
    };
    this.DoBeforeClose = function () {
      if ((this.FBeforeClose != null) && !(3 in this.FComponentState)) this.FBeforeClose(this);
    };
    this.DoBeforeOpen = function () {
      if (this.FBeforeOpen != null) this.FBeforeOpen(this);
    };
    this.DoBeforePost = function () {
      if (this.FBeforePost != null) this.FBeforePost(this);
    };
    this.DoBeforeScroll = function () {
      if (this.FBeforeScroll != null) this.FBeforeScroll(this);
    };
    this.DoOnCalcFields = function () {
      if (this.FOnCalcFields != null) this.FOnCalcFields(this);
    };
    this.DoBeforeLoad = function () {
      if (this.FBeforeLoad != null) this.FBeforeLoad(this);
    };
    this.DoAfterLoad = function () {
      if (this.FAfterLoad != null) this.FAfterLoad(this);
    };
    this.GetCalcFields = function (Buffer) {
      var OldState = 0;
      if ((this.FCalcFieldsCount > 0) || this.FInternalCalcFields) {
        OldState = this.FState;
        this.FState = 5;
        try {
          this.CalculateFields(Buffer);
        } finally {
          this.FState = OldState;
        };
      };
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      Result = $mod.DefaultFieldClasses[FieldType];
      return Result;
    };
    this.GetfieldCount = function () {
      var Result = 0;
      Result = this.FFieldList.GetCount();
      return Result;
    };
    this.GetFieldValues = function (FieldName) {
      var Result = undefined;
      var i = 0;
      var FieldList = null;
      var A = [];
      FieldList = $lt65.$create("Create$1");
      try {
        this.GetFieldList(FieldList,FieldName);
        if (FieldList.GetCount() > 1) {
          A = rtl.arraySetLength(A,undefined,FieldList.GetCount());
          for (var $l = 0, $end = FieldList.GetCount() - 1; $l <= $end; $l++) {
            i = $l;
            A[i] = rtl.getObject(FieldList.Get(i)).GetAsJSValue();
          };
          Result = A;
        } else Result = this.FieldByName(FieldName).GetAsJSValue();
      } finally {
        FieldList = rtl.freeLoc(FieldList);
      };
      return Result;
    };
    this.GetNextRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetNextRecord()) Result += 1;
      return Result;
    };
    this.GetNextRecord = function () {
      var Result = false;
      var T = $lt45.$new();
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FRecordCount - 1);
      Result = this.GetRecord(this.FBuffers[this.FBufferCount],1,true) === 0;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        if (this.FRecordCount === this.FBufferCount) {
          this.ShiftBuffersBackward()}
         else {
          this.FRecordCount += 1;
          this.FCurrentRecord = this.FRecordCount - 1;
          T.$assign(this.FBuffers[this.FCurrentRecord]);
          this.FBuffers[this.FCurrentRecord].$assign(this.FBuffers[this.FBufferCount]);
          this.FBuffers[this.FBufferCount].$assign(T);
        };
      } else this.CursorPosChanged();
      return Result;
    };
    this.GetPriorRecords = function () {
      var Result = 0;
      Result = 0;
      while ((this.FRecordCount < this.FBufferCount) && this.GetPriorRecord()) Result += 1;
      return Result;
    };
    this.GetPriorRecord = function () {
      var Result = false;
      this.CheckBiDirectional();
      if (this.FRecordCount > 0) this.SetCurrentRecord(0);
      Result = this.GetRecord(this.FBuffers[this.FBufferCount],2,true) === 0;
      if (Result) {
        if (this.FRecordCount === 0) this.ActivateBuffers();
        this.ShiftBuffersForward();
        if (this.FRecordCount < this.FBufferCount) this.FRecordCount += 1;
      } else this.CursorPosChanged();
      return Result;
    };
    this.InitRecord = function (Buffer) {
      this.InternalInitRecord(Buffer);
      this.ClearCalcFields(Buffer);
    };
    this.InternalCancel = function () {
    };
    this.OpenCursor = function (InfoQuery) {
      if (InfoQuery) {
        this.InternalInitFieldDefs()}
       else if (this.FState !== 12) this.DoInternalOpen();
    };
    this.OpenCursorcomplete = function () {
      try {
        if (this.FState === 12) this.DoInternalOpen();
      } finally {
        if (this.FInternalOpenComplete) {
          this.SetState(1);
          this.DoAfterOpen();
          if (!this.IsEmpty()) this.DoAfterScroll();
        } else {
          this.SetState(0);
          this.CloseCursor();
        };
      };
    };
    this.RefreshInternalCalcFields = function (Buffer) {
    };
    this.RestoreState = function (Value) {
      this.FState = Value;
      this.FDisableControlsCount -= 1;
    };
    this.SetActive = function (Value) {
      if (Value && (this.FState === 0)) {
        if (0 in this.FComponentState) {
          this.FOpenAfterRead = true;
          return;
        } else {
          this.DoBeforeOpen();
          this.FEnableControlsEvent = 4;
          this.FInternalCalcFields = false;
          try {
            this.FDefaultFields = this.GetfieldCount() === 0;
            this.OpenCursor(false);
          } finally {
            if (this.FState !== 12) this.OpenCursorcomplete();
          };
        };
        this.FModified = false;
      } else if (!Value && (this.FState !== 0)) {
        this.DoBeforeClose();
        this.SetState(0);
        this.DoneChangeList();
        this.CloseCursor();
        this.DoAfterClose();
        this.FModified = false;
      };
    };
    this.SetBufListSize = function (Value) {
      var I = 0;
      if (Value < 0) Value = 0;
      if (Value === this.FBufferCount) return;
      if (Value > this.FBufferCount) {
        this.FBuffers = rtl.arraySetLength(this.FBuffers,$lt45,Value + 1);
        for (var $l = this.FBufferCount, $end = Value; $l <= $end; $l++) {
          I = $l;
          this.FBuffers[I].$assign(this.AllocRecordBuffer());
        };
      } else if (Value < this.FBufferCount) if ((Value >= 0) && (this.FActiveRecord > (Value - 1))) {
        for (var $l1 = 0, $end1 = this.FActiveRecord - Value; $l1 <= $end1; $l1++) {
          I = $l1;
          this.ShiftBuffersBackward();
        };
        this.FActiveRecord = Value - 1;
      };
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$lt45,Value + 1);
      this.FBufferCount = Value;
      if (this.FRecordCount > this.FBufferCount) this.FRecordCount = this.FBufferCount;
    };
    this.SetCurrentRecord = function (Index) {
      if (this.FCurrentRecord !== Index) {
        if (!this.FIsUniDirectional) {
          var $tmp = this.GetBookmarkFlag($lt45.$clone(this.FBuffers[Index]));
          if ($tmp === 0) {
            this.InternalSetToRecord($lt45.$clone(this.FBuffers[Index]))}
           else if ($tmp === 1) {
            this.InternalFirst()}
           else if ($tmp === 2) this.InternalLast();
        };
        this.FCurrentRecord = Index;
      };
    };
    this.SetFiltered = function (Value) {
      if (Value) this.CheckBiDirectional();
      this.FFiltered = Value;
    };
    this.SetFilterText = function (Value) {
      this.FFilterText = Value;
    };
    this.SetModified = function (Value) {
      this.FModified = Value;
    };
    this.SetName = function (NewName) {
      var $Self = this;
      function CheckName(FieldName) {
        var Result = "";
        var i = 0;
        var j = 0;
        Result = FieldName;
        i = 0;
        j = 0;
        while (i < $Self.FFieldList.GetCount()) {
          if (Result === $Self.FFieldList.GetField(i).FFieldName) {
            j += 1;
            Result = FieldName + $lp2(j);
          } else i += 1;
        };
        return Result;
      };
      var i = 0;
      var nm = "";
      var old = "";
      if ($Self.FName === NewName) return;
      old = $Self.FName;
      $lt60.SetName.call(this,NewName);
      if (4 in this.FComponentState) for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        nm = old + this.FFieldList.GetField(i).FFieldName;
        if ($lp1(this.FFieldList.GetField(i).FName,1,nm.length) === nm) this.FFieldList.GetField(i).SetName(CheckName(NewName + this.FFieldList.GetField(i).FFieldName));
      };
    };
    this.SetOnFilterRecord = function (Value) {
      this.CheckBiDirectional();
      this.FOnFilterRecord = Value;
    };
    this.SetState = function (Value) {
      if (Value !== this.FState) {
        this.FState = Value;
        if (Value === 1) this.FModified = false;
        this.DataEvent(6,0);
      };
    };
    this.SetTempState = function (Value) {
      var Result = 0;
      Result = this.FState;
      this.FState = Value;
      this.FDisableControlsCount += 1;
      return Result;
    };
    this.AllocRecordBuffer = function () {
      var Result = $lt45.$new();
      Result.data = null;
      Result.state = 0;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
    };
    this.GetBookmarkData = function (Buffer, Data) {
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.GetDataSource = function () {
      var Result = null;
      Result = null;
      return Result;
    };
    this.InternalFirst = function () {
    };
    this.InternalInitRecord = function (Buffer) {
    };
    this.InternalLast = function () {
    };
    this.InternalPost = function () {
      var $Self = this;
      function CheckRequiredFields() {
        var I = 0;
        for (var $l = 0, $end = $Self.FFieldList.GetCount() - 1; $l <= $end; $l++) {
          I = $l;
          var $with = $Self.FFieldList.GetField(I);
          if ($with.FRequired && !$with.FReadOnly && ($with.FFieldKind === 0) && !($with.FDataType === 9) && $with.GetIsNull()) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SNeedField"),$lp(18,$with.GetDisplayName()),$Self);
        };
      };
      CheckRequiredFields();
    };
    this.InternalSetToRecord = function (Buffer) {
    };
    this.Notification = function (AComponent, Operation) {
      $lt60.Notification.call(this,AComponent,Operation);
      if ((Operation === 1) && (AComponent === this.FDataProxy)) this.FDataProxy = null;
    };
    this.SetDataSetField = function (Value) {
      if (Value === this.FDataSetField) return;
      if ((Value !== null) && ((Value.FDataSet === this) || ((Value.FDataSet.GetDataSource() !== null) && (Value.FDataSet.GetDataSource().FDataSet === this)))) $mod.DatabaseError$1(rtl.getResStr($lm2,"SCircularDataLink"),this);
      if ((Value != null) && !this.$class.InheritsFrom(Value.FDataSet.FNestedDataSetClass)) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SNestedDataSetClass"),$lp(18,Value.FDataSet.FNestedDataSetClass.$classname),this);
      if (this.GetActive()) this.Close();
      if (this.FDataSetField != null) this.FDataSetField.AssignNestedDataSet(null);
      this.FDataSetField = Value;
      if (Value != null) {
        Value.AssignNestedDataSet(this);
        if (Value.FDataSet.GetActive()) this.Open();
      };
    };
    this.GetFieldData = function (Field) {
      var Result = undefined;
      Result = this.GetFieldData$1(Field,$lt45.$clone(this.ActiveBuffer()));
      return Result;
    };
    this.SetFieldData = function (Field, AValue) {
      this.SetFieldData$1(Field,this.FBuffers[this.FActiveRecord],AValue);
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      Result = Buffer.data[Field.FFieldName];
      if ($lp8(Result)) Result = null;
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      Buffer.data[Field.FFieldName] = AValue;
    };
    this.FieldDefsClass = function () {
      var Result = null;
      Result = $lt10;
      return Result;
    };
    this.FieldsClass = function () {
      var Result = null;
      Result = $lt35;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt60.Create$1.call(this,AOwner);
      this.FFieldDefs = this.$class.FieldDefsClass().$create("Create$4",[this]);
      this.FFieldList = this.$class.FieldsClass().$create("Create$1",[this]);
      this.FDataSources = $lt59.$create("Create");
      this.FConstraints = $lt34.$create("Create$2",[this]);
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$lt45,1);
      this.FActiveRecord = 0;
      this.FEOF = true;
      this.FBOF = true;
      this.FIsUniDirectional = false;
      this.FAutoCalcFields = true;
      this.FDataRequestID = 0;
      this.FNestedDataSetClass = this.$class.ClassType();
      return this;
    };
    this.Destroy = function () {
      var i = 0;
      this.SetActive(false);
      this.SetDataSetField(null);
      rtl.free(this,"FFieldDefs");
      rtl.free(this,"FFieldList");
      rtl.free(this,"FNestedDataSets");
      var $with = this.FDataSources;
      while ($with.FCount > 0) rtl.getObject($with.Get($with.FCount - 1)).SetDataSet(null);
      $with.$destroy("Destroy");
      for (var $l = 0, $end = this.FBufferCount; $l <= $end; $l++) {
        i = $l;
        this.FreeRecordBuffer(this.FBuffers[i]);
      };
      rtl.free(this,"FConstraints");
      this.FBuffers = rtl.arraySetLength(this.FBuffers,$lt45,1);
      $lt60.Destroy.call(this);
    };
    this.ActiveBuffer = function () {
      var Result = $lt45.$new();
      if (this.FActiveRecord !== -1) {
        Result.$assign(this.FBuffers[this.FActiveRecord])}
       else Result.$assign($lt45.$new());
      return Result;
    };
    this.ConvertToDateTime = function (aField, aValue, ARaiseException) {
      var Result = 0.0;
      Result = this.$class.DefaultConvertToDateTime(aField,aValue,ARaiseException);
      return Result;
    };
    this.ConvertDateTimeToNative = function (aField, aValue) {
      var Result = undefined;
      Result = this.$class.DefaultConvertDateTimeToNative(aField,aValue);
      return Result;
    };
    this.DefaultConvertToDateTime = function (aField, aValue, ARaiseException) {
      var Result = 0.0;
      Result = 0;
      if (rtl.isString(aValue)) {
        if (!$lp21("" + aValue,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})) throw $lt66.$create("CreateFmt",[rtl.getResStr($lm2,"SErrInvalidDateTime"),$lp(18,"" + aValue)]);
      } else if (rtl.isNumber(aValue)) {
        Result = rtl.getNumber(aValue)}
       else if ($lp22(aValue)) Result = $lp23(aValue);
      return Result;
    };
    this.DefaultConvertDateTimeToNative = function (aField, aValue) {
      var Result = undefined;
      Result = $lp24(aValue);
      return Result;
    };
    this.BlobDataToBytes = function (aValue) {
      var Result = [];
      Result = this.$class.DefaultBlobDataToBytes(aValue);
      return Result;
    };
    this.DefaultBlobDataToBytes = function (aValue) {
      var Result = [];
      var S = "";
      var I = 0;
      var J = 0;
      var L = 0;
      Result = rtl.arraySetLength(Result,0,0);
      if (rtl.isString(aValue)) {
        S = "" + aValue;
        L = S.length;
        Result = rtl.arraySetLength(Result,0,rtl.trunc((L + 1) / 2));
        I = 1;
        J = 0;
        while (I < L) {
          Result[J] = $lp12("$" + $lp1(S,I,2));
          I += 2;
          J += 1;
        };
      };
      return Result;
    };
    this.BytesToBlobData = function (aValue) {
      var Result = undefined;
      Result = this.$class.DefaultBytesToBlobData(rtl.arrayRef(aValue));
      return Result;
    };
    this.DefaultBytesToBlobData = function (aValue) {
      var Result = undefined;
      var S = "";
      var I = 0;
      if (rtl.length(aValue) === 0) {
        Result = null}
       else {
        S = "";
        for (var $l = 0, $end = rtl.length(aValue) - 1; $l <= $end; $l++) {
          I = $l;
          S = S.concat($lp25(aValue[I],2));
        };
        Result = S;
      };
      return Result;
    };
    this.Cancel = function () {
      if (this.FState in rtl.createSet(2,3)) {
        this.DataEvent(7,0);
        this.DoBeforeCancel();
        this.UpdateCursorPos();
        this.InternalCancel();
        if ((this.FState === 3) && (this.FRecordCount === 1)) {
          this.FEOF = true;
          this.FBOF = true;
          this.FRecordCount = 0;
          this.InitRecord(this.FBuffers[this.FActiveRecord]);
          this.SetState(1);
          this.DataEvent(2,0);
        } else {
          this.SetState(1);
          this.SetCurrentRecord(this.FActiveRecord);
          this.Resync({});
        };
        this.DoAfterCancel();
      };
    };
    this.CheckBrowseMode = function () {
      this.CheckActive();
      this.DataEvent(7,0);
      var $tmp = this.FState;
      if (($tmp === 2) || ($tmp === 3)) {
        this.UpdateRecord();
        if (this.FModified) {
          this.Post()}
         else this.Cancel();
      } else if ($tmp === 4) this.Post();
    };
    this.Close = function () {
      this.SetActive(false);
    };
    this.ControlsDisabled = function () {
      var Result = false;
      Result = this.FDisableControlsCount > 0;
      return Result;
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      Result = 0;
      return Result;
    };
    this.CursorPosChanged = function () {
      this.FCurrentRecord = -1;
    };
    this.DisableControls = function () {
      if (this.FDisableControlsCount === 0) {
        this.FDisableControlsState = this.FState;
        this.FEnableControlsEvent = 2;
      };
      this.FDisableControlsCount += 1;
    };
    this.EnableControls = function () {
      if (this.FDisableControlsCount > 0) this.FDisableControlsCount -= 1;
      if (this.FDisableControlsCount === 0) {
        if (this.FState !== this.FDisableControlsState) this.DataEvent(6,0);
        if ((this.FState !== 0) && (this.FDisableControlsState !== 0)) this.DataEvent(this.FEnableControlsEvent,0);
      };
    };
    this.FieldByName = function (FieldName) {
      var Result = null;
      Result = this.FindField(FieldName);
      if (Result === null) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SFieldNotFound"),$lp(18,FieldName),this);
      return Result;
    };
    this.FindField = function (FieldName) {
      var Result = null;
      Result = this.FFieldList.FindField(FieldName);
      return Result;
    };
    this.First = function () {
      this.CheckBrowseMode();
      this.DoBeforeScroll();
      if (!this.FIsUniDirectional) {
        this.ClearBuffers()}
       else if (!this.FBOF) {
        this.SetActive(false);
        this.SetActive(true);
      };
      try {
        this.InternalFirst();
        if (!this.FIsUniDirectional) this.GetNextRecords();
      } finally {
        this.FBOF = true;
        this.DataEvent(2,0);
        this.DoAfterScroll();
      };
    };
    this.GetBookmark = function () {
      var Result = $lt37.$new();
      if (this.BookmarkAvailable()) {
        this.GetBookmarkData($lt45.$clone(this.ActiveBuffer()),Result)}
       else Result.Data = null;
      return Result;
    };
    this.GetFieldList = function (List, FieldNames) {
      var F = null;
      var N = "";
      var StrPos = 0;
      if ((FieldNames === "") || (List === null)) return;
      StrPos = 1;
      do {
        N = $mod.ExtractFieldName(FieldNames,{get: function () {
            return StrPos;
          }, set: function (v) {
            StrPos = v;
          }});
        F = this.FieldByName(N);
        List.Add(F);
      } while (!(StrPos > FieldNames.length));
    };
    this.GetFieldList$1 = function (List, FieldNames) {
      var F = null;
      var N = "";
      var StrPos = 0;
      if ((FieldNames === "") || (List === null)) return;
      StrPos = 1;
      do {
        N = $mod.ExtractFieldName(FieldNames,{get: function () {
            return StrPos;
          }, set: function (v) {
            StrPos = v;
          }});
        F = this.FieldByName(N);
        List.Add(F);
      } while (!(StrPos > FieldNames.length));
    };
    this.IsEmpty = function () {
      var Result = false;
      Result = this.FBOF && this.FEOF && !(this.FState === 3);
      return Result;
    };
    this.Lookup = function (KeyFields, KeyValues, ResultFields) {
      var Result = undefined;
      this.CheckBiDirectional();
      Result = null;
      return Result;
    };
    this.MoveBy = function (Distance) {
      var $Self = this;
      var Result = 0;
      var TheResult = 0;
      function ScrollForward() {
        var Result = 0;
        Result = 0;
        $Self.FBOF = false;
        while ((Distance > 0) && !$Self.FEOF) {
          if ($Self.FActiveRecord < ($Self.FRecordCount - 1)) {
            $Self.FActiveRecord += 1;
            Distance -= 1;
            TheResult += 1;
          } else {
            if ($Self.GetNextRecord()) {
              Distance -= 1;
              Result -= 1;
              TheResult += 1;
            } else {
              $Self.FEOF = true;
              $Self.DoLoad(rtl.createSet(0,2),null);
            };
          };
        };
        return Result;
      };
      function ScrollBackward() {
        var Result = 0;
        $Self.CheckBiDirectional();
        Result = 0;
        $Self.FEOF = false;
        while ((Distance < 0) && !$Self.FBOF) {
          if ($Self.FActiveRecord > 0) {
            $Self.FActiveRecord -= 1;
            Distance += 1;
            TheResult -= 1;
          } else {
            if ($Self.GetPriorRecord()) {
              Distance += 1;
              Result += 1;
              TheResult -= 1;
            } else $Self.FBOF = true;
          };
        };
        return Result;
      };
      var Scrolled = 0;
      this.CheckBrowseMode();
      Result = 0;
      TheResult = 0;
      this.DoBeforeScroll();
      if ((Distance === 0) || ((Distance > 0) && this.FEOF) || ((Distance < 0) && this.FBOF)) return Result;
      try {
        Scrolled = 0;
        if (Distance > 0) {
          Scrolled = ScrollForward()}
         else Scrolled = ScrollBackward();
      } finally {
        this.DataEvent(3,Scrolled);
        this.DoAfterScroll();
        Result = TheResult;
      };
      return Result;
    };
    this.Next = function () {
      if (this.FBlockReadSize > 0) {
        this.BlockReadNext()}
       else this.MoveBy(1);
    };
    this.Open = function () {
      this.SetActive(true);
    };
    var UpdateStates = [0,1];
    this.Post = function () {
      var R = null;
      var WasInsert = false;
      this.UpdateRecord();
      if (this.FState in rtl.createSet(2,3)) {
        this.DataEvent(7,0);
        this.DoBeforePost();
        WasInsert = this.FState === 3;
        if (!this.TryDoing(rtl.createCallback(this,"InternalPost"),this.FOnPostError)) return;
        this.CursorPosChanged();
        this.SetState(1);
        this.Resync({});
        R = this.AddToChangeList(UpdateStates[+WasInsert]);
        if (R != null) R.FBookmark.$assign(this.GetBookmark());
        this.DoAfterPost();
      } else if (this.FState !== 4) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SNotEditing"),$lp(18,this.FName),this);
    };
    this.Resync = function (Mode) {
      var i = 0;
      var count = 0;
      if (this.FIsUniDirectional) return;
      if (this.GetRecord(this.FBuffers[0],0,false) !== 0) if (0 in Mode) {
        $mod.DatabaseError$1(rtl.getResStr($lm2,"SNoSuchRecord"),this)}
       else if ((this.GetRecord(this.FBuffers[0],1,true) !== 0) && (this.GetRecord(this.FBuffers[0],2,true) !== 0)) {
        this.ClearBuffers();
        this.InternalInitRecord(this.FBuffers[this.FActiveRecord]);
        this.DataEvent(2,0);
        return;
      };
      this.FCurrentRecord = 0;
      this.FEOF = false;
      this.FBOF = false;
      if (1 in Mode) {
        count = rtl.trunc(this.FRecordCount / 2)}
       else count = this.FActiveRecord;
      i = 0;
      this.FRecordCount = 1;
      this.FActiveRecord = 0;
      while ((i < count) && this.GetPriorRecord()) i += 1;
      this.FActiveRecord = i;
      this.GetNextRecords();
      if (this.FRecordCount < this.FBufferCount) this.FActiveRecord = this.FActiveRecord + this.GetPriorRecords();
      this.DataEvent(2,0);
    };
    this.UpdateCursorPos = function () {
      if (this.FRecordCount > 0) this.SetCurrentRecord(this.FActiveRecord);
    };
    this.UpdateRecord = function () {
      if (!(this.FState in $mod.dsEditModes)) $mod.DatabaseErrorFmt$1(rtl.getResStr($lm2,"SNotEditing"),$lp(18,this.FName),this);
      this.DataEvent(5,0);
    };
  });
  rtl.createClass(this,"TDataLink",$lt57,function () {
    $lt47 = this;
    this.$init = function () {
      $lt57.$init.call(this);
      this.FFirstRecord = 0;
      this.FBufferCount = 0;
      this.FActive = false;
      this.FDataSourceFixed = false;
      this.FEditing = false;
      this.FReadOnly = false;
      this.FUpdatingRecord = false;
      this.FVisualControl = false;
      this.FDataSource = null;
    };
    this.$final = function () {
      this.FDataSource = undefined;
      $lt57.$final.call(this);
    };
    this.CalcFirstRecord = function (Index) {
      var Result = 0;
      if (this.FDataSource.FDataSet.FActiveRecord > ((this.FFirstRecord + Index + this.FBufferCount) - 1)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - ((this.FFirstRecord + Index + this.FBufferCount) - 1)}
       else if (this.FDataSource.FDataSet.FActiveRecord < (this.FFirstRecord + Index)) {
        Result = this.FDataSource.FDataSet.FActiveRecord - (this.FFirstRecord + Index)}
       else Result = 0;
      this.FFirstRecord += Index + Result;
      return Result;
    };
    this.CalcRange = function () {
      var aMax = 0;
      var aMin = 0;
      aMin = (this.GetDataset().FActiveRecord - this.FBufferCount) + 1;
      if (aMin < 0) aMin = 0;
      aMax = this.GetDataset().FBufferCount - this.FBufferCount;
      if (aMax < 0) aMax = 0;
      if (aMax > this.GetDataset().FActiveRecord) aMax = this.GetDataset().FActiveRecord;
      if (this.FFirstRecord < aMin) this.FFirstRecord = aMin;
      if (this.FFirstRecord > aMax) this.FFirstRecord = aMax;
      if ((this.FFirstRecord !== 0) && ((this.GetDataset().FActiveRecord - this.FFirstRecord) < (this.FBufferCount - 1))) this.FFirstRecord -= 1;
    };
    this.CheckActiveAndEditing = function () {
      var B = false;
      B = (this.FDataSource != null) && !(this.FDataSource.FState in rtl.createSet(0,12));
      this.SetActive(B);
      B = (this.FDataSource != null) && (this.FDataSource.FState in $mod.dsEditModes) && !this.FReadOnly;
      if (B !== this.FEditing) {
        this.FEditing = B;
        this.EditingChanged();
      };
    };
    this.GetDataset = function () {
      var Result = null;
      if (this.FDataSource != null) {
        Result = this.FDataSource.FDataSet}
       else Result = null;
      return Result;
    };
    this.SetActive = function (AActive) {
      if (this.FActive !== AActive) {
        this.FActive = AActive;
        this.ActiveChanged();
      };
    };
    this.SetDataSource = function (Value) {
      if (this.FDataSource === Value) return;
      if (!this.FDataSourceFixed) {
        if (this.FDataSource != null) {
          this.FDataSource.UnregisterDataLink(this);
          this.FDataSource = null;
          this.CheckActiveAndEditing();
        };
        this.FDataSource = Value;
        if (this.FDataSource != null) {
          this.FDataSource.RegisterDataLink(this);
          this.CheckActiveAndEditing();
        };
      };
    };
    this.ActiveChanged = function () {
      this.FFirstRecord = 0;
    };
    this.CheckBrowseMode = function () {
    };
    this.DataEvent = function (Event, Info) {
      if (Event === 6) {
        this.CheckActiveAndEditing()}
       else if (this.FActive) {
        var $tmp = Event;
        if (($tmp === 0) || ($tmp === 1)) {
          if (!this.FUpdatingRecord) this.RecordChanged(rtl.getObject(Info))}
         else if ($tmp === 2) {
          this.SetActive(this.FDataSource.FDataSet.GetActive());
          this.CalcRange();
          this.CalcFirstRecord(rtl.trunc(Info));
          this.DataSetChanged();
        } else if ($tmp === 3) {
          this.DataSetScrolled(this.CalcFirstRecord(rtl.trunc(Info)))}
         else if ($tmp === 4) {
          this.CalcFirstRecord(rtl.trunc(Info));
          this.LayoutChanged();
        } else if ($tmp === 5) {
          this.UpdateRecord()}
         else if ($tmp === 7) {
          this.CheckBrowseMode()}
         else if ($tmp === 10) this.FocusControl(Info);
      };
    };
    this.DataSetChanged = function () {
      this.RecordChanged(null);
    };
    this.DataSetScrolled = function (Distance) {
      this.DataSetChanged();
    };
    this.EditingChanged = function () {
    };
    this.FocusControl = function (Field) {
    };
    this.GetBufferCount = function () {
      var Result = 0;
      Result = this.FBufferCount;
      return Result;
    };
    this.LayoutChanged = function () {
      this.DataSetChanged();
    };
    this.RecordChanged = function (Field) {
    };
    this.UpdateData = function () {
    };
    this.Destroy = function () {
      this.FActive = false;
      this.FEditing = false;
      this.FDataSourceFixed = false;
      this.SetDataSource(null);
      $lt58.Destroy.call(this);
    };
    this.UpdateRecord = function () {
      this.FUpdatingRecord = true;
      try {
        this.UpdateData();
      } finally {
        this.FUpdatingRecord = false;
      };
    };
  });
  this.$rtti.$MethodVar("TDataChangeEvent",{procsig: rtl.newTIProcSig([["Sender",$lm3.$rtti["TObject"]],["Field",this.$rtti["TField"]]]), methodkind: 0});
  rtl.createClass(this,"TDataSource",$lt60,function () {
    $lt48 = this;
    this.$init = function () {
      $lt60.$init.call(this);
      this.FDataSet = null;
      this.FDataLinks = null;
      this.FEnabled = false;
      this.FAutoEdit = false;
      this.FState = 0;
      this.FOnStateChange = null;
      this.FOnDataChange = null;
      this.FOnUpdateData = null;
    };
    this.$final = function () {
      this.FDataSet = undefined;
      this.FDataLinks = undefined;
      this.FOnStateChange = undefined;
      this.FOnDataChange = undefined;
      this.FOnUpdateData = undefined;
      $lt60.$final.call(this);
    };
    this.DistributeEvent = function (Event, Info) {
      var i = 0;
      var $with = this.FDataLinks;
      for (var $l = 0, $end = $with.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        var $with1 = rtl.getObject($with.Get(i));
        if (!$with1.FVisualControl) $with1.DataEvent(Event,Info);
      };
      for (var $l1 = 0, $end1 = $with.GetCount() - 1; $l1 <= $end1; $l1++) {
        i = $l1;
        var $with2 = rtl.getObject($with.Get(i));
        if ($with2.FVisualControl) $with2.DataEvent(Event,Info);
      };
    };
    this.RegisterDataLink = function (DataLink) {
      this.FDataLinks.Add(DataLink);
      if (this.FDataSet != null) this.FDataSet.RecalcBufListSize();
    };
    var OnDataChangeEvents = rtl.createSet(1,2,3,4,6);
    this.ProcessEvent = function (Event, Info) {
      var NeedDataChange = false;
      var FLastState = 0;
      if (Event === 6) {
        NeedDataChange = this.FState === 0;
        FLastState = this.FState;
        if ((this.FDataSet != null) && this.FEnabled) {
          this.FState = this.FDataSet.FState}
         else this.FState = 0;
        if (this.FState === FLastState) return;
      } else NeedDataChange = true;
      this.DistributeEvent(Event,Info);
      if (!(3 in this.FComponentState)) {
        if (Event === 6) this.DoStateChange();
        if ((Event in OnDataChangeEvents) && NeedDataChange) this.DoDataChange(null);
        if (Event === 0) this.DoDataChange(Info);
        if (Event === 5) this.DoUpdateData();
      };
    };
    this.SetDataSet = function (ADataSet) {
      if (this.FDataSet !== null) {
        this.FDataSet.UnRegisterDataSource(this);
        this.FDataSet = null;
        this.ProcessEvent(6,0);
      };
      if (ADataSet !== null) {
        ADataSet.RegisterDataSource(this);
        this.FDataSet = ADataSet;
        this.ProcessEvent(6,0);
      };
    };
    this.SetEnabled = function (Value) {
      this.FEnabled = Value;
      this.ProcessEvent(6,0);
    };
    this.UnregisterDataLink = function (DataLink) {
      this.FDataLinks.Remove(DataLink);
      if (this.FDataSet !== null) this.FDataSet.RecalcBufListSize();
    };
    this.DoDataChange = function (Info) {
      if (this.FOnDataChange != null) this.FOnDataChange(this,Info);
    };
    this.DoStateChange = function () {
      if (this.FOnStateChange != null) this.FOnStateChange(this);
    };
    this.DoUpdateData = function () {
      if (this.FOnUpdateData != null) this.FOnUpdateData(this);
    };
    this.Create$1 = function (AOwner) {
      $lt60.Create$1.call(this,AOwner);
      this.FDataLinks = $lt65.$create("Create$1");
      this.FEnabled = true;
      this.FAutoEdit = true;
      return this;
    };
    this.Destroy = function () {
      this.FOnStateChange = null;
      this.SetDataSet(null);
      var $with = this.FDataLinks;
      while ($with.GetCount() > 0) rtl.getObject($with.Get($with.GetCount() - 1)).SetDataSource(null);
      rtl.free(this,"FDataLinks");
      $lt60.Destroy.call(this);
    };
    var $r = this.$rtti;
    $r.addProperty("AutoEdit",0,rtl.boolean,"FAutoEdit","FAutoEdit",{Default: true});
    $r.addProperty("DataSet",2,$mod.$rtti["TDataSet"],"FDataSet","SetDataSet");
    $r.addProperty("Enabled",2,rtl.boolean,"FEnabled","SetEnabled",{Default: true});
    $r.addProperty("OnStateChange",0,$lm1.$rtti["TNotifyEvent"],"FOnStateChange","FOnStateChange");
    $r.addProperty("OnDataChange",0,$mod.$rtti["TDataChangeEvent"],"FOnDataChange","FOnDataChange");
    $r.addProperty("OnUpdateData",0,$lm1.$rtti["TNotifyEvent"],"FOnUpdateData","FOnUpdateData");
  });
  $lt49 = this.TDataRequestResult = {"0": "rrFail", rrFail: 0, "1": "rrEOF", rrEOF: 1, "2": "rrOK", rrOK: 2};
  rtl.createClass(this,"TDataRequest",$lt58,function () {
    $lt50 = this;
    this.$init = function () {
      $lt58.$init.call(this);
      this.FBookmark = $lt37.$new();
      this.FDataset = null;
      this.FErrorMsg = "";
      this.FEvent = null;
      this.FLoadOptions = {};
      this.FRequestID = 0;
      this.FSuccess = 0;
      this.FData = undefined;
      this.FAfterRequest = null;
      this.FDataProxy = null;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FEvent = undefined;
      this.FLoadOptions = undefined;
      this.FAfterRequest = undefined;
      this.FDataProxy = undefined;
      $lt58.$final.call(this);
    };
    this.Create$1 = function (aDataProxy, aOptions, aAfterRequest, aAfterLoad) {
      this.FDataProxy = aDataProxy;
      this.FLoadOptions = rtl.refSet(aOptions);
      this.FEvent = aAfterLoad;
      this.FAfterRequest = aAfterRequest;
      return this;
    };
  });
  rtl.createClass(this,"TRecordUpdateDescriptor",$lt58,function () {
    $lt51 = this;
    this.$init = function () {
      $lt58.$init.call(this);
      this.FBookmark = $lt37.$new();
      this.FData = undefined;
      this.FDataset = null;
      this.FProxy = null;
      this.FStatus = 0;
    };
    this.$final = function () {
      this.FBookmark = undefined;
      this.FDataset = undefined;
      this.FProxy = undefined;
      $lt58.$final.call(this);
    };
    this.Create$1 = function (aProxy, aDataset, aBookmark, AData, AStatus) {
      this.FDataset = aDataset;
      this.FBookmark.$assign(aBookmark);
      this.FData = AData;
      this.FStatus = AStatus;
      this.FProxy = aProxy;
      return this;
    };
  });
  rtl.createClass(this,"TDataProxy",$lt60,function () {
    $lt52 = this;
    this.GetDataRequestClass = function () {
      var Result = null;
      Result = $lt50;
      return Result;
    };
    this.GetUpdateDescriptorClass = function () {
      var Result = null;
      Result = $lt51;
      return Result;
    };
    this.GetDataRequest = function (aOptions, aAfterRequest, aAfterLoad) {
      var Result = null;
      Result = this.GetDataRequestClass().$create("Create$1",[this,rtl.refSet(aOptions),aAfterRequest,aAfterLoad]);
      return Result;
    };
    this.GetUpdateDescriptor = function (aDataset, aBookmark, AData, AStatus) {
      var Result = null;
      Result = this.GetUpdateDescriptorClass().$create("Create$1",[this,aDataset,$lt37.$clone(aBookmark),AData,AStatus]);
      return Result;
    };
  });
  this.Fieldtypenames = ["Unknown","String","Integer","NativeInt","Boolean","Float","Date","Time","DateTime","AutoInc","Blob","Memo","FixedChar","Variant","Dataset"];
  this.DefaultFieldClasses = [$lt13,$lt14,$lt16,$lt17,$lt20,$lt19,$lt22,$lt23,$lt21,$lt18,$lt26,$lt27,$lt14,$lt28,null];
  this.dsEditModes = rtl.createSet(2,3,4);
  this.ftBlobTypes = rtl.createSet(10,11);
  this.DatabaseError = function (Msg) {
    throw $lt4.$create("Create$1",[Msg]);
  };
  this.DatabaseError$1 = function (Msg, Comp) {
    if ((Comp != null) && (Comp.FName !== "")) {
      throw $lt4.$create("CreateFmt",["%s : %s",$lp(18,Comp.FName,18,Msg)])}
     else $mod.DatabaseError(Msg);
  };
  this.DatabaseErrorFmt = function (Fmt, Args) {
    throw $lt4.$create("CreateFmt",[Fmt,Args]);
  };
  this.DatabaseErrorFmt$1 = function (Fmt, Args, Comp) {
    if (Comp != null) {
      throw $lt4.$create("CreateFmt",[$lp26("%s : %s",$lp(18,Comp.FName,18,Fmt)),Args])}
     else $mod.DatabaseErrorFmt(Fmt,Args);
  };
  this.ExtractFieldName = function (Fields, Pos) {
    var Result = "";
    var i = 0;
    var FieldsLength = 0;
    i = Pos.get();
    FieldsLength = Fields.length;
    while ((i <= FieldsLength) && (Fields.charAt(i - 1) !== ";")) i += 1;
    Result = $lp27($lp1(Fields,Pos.get(),i - Pos.get()));
    if ((i <= FieldsLength) && (Fields.charAt(i - 1) === ";")) i += 1;
    Pos.set(i);
    return Result;
  };
  $mod.$implcode = function () {
    $lm2 = pas.DBConst;
    $impl.DefaultBufferCount = 10;
    $impl.SBoolean = "Boolean";
    $impl.SDateTime = "TDateTime";
    $impl.SInteger = "Integer";
    $impl.SLargeInt = "NativeInt";
    $impl.SJSValue = "JSValue";
  };
},["DBConst","TypInfo"]);
rtl.module("contnrs",["System","SysUtils","Classes"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lm = pas.System;
  var $lt7 = $lm.TObject;
  var $lm1 = pas.Classes;
  var $lt8 = $lm1.TFPList;
  var $lp = $lm.VarRecs;
  var $lm2 = pas.SysUtils;
  var $lp1 = $lm2.FreeAndNil;
  var $lt9 = $lm2.Exception;
  rtl.createClass(this,"TFPObjectList",$lt7,function () {
    $lt = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FFreeObjects = false;
      this.FList = null;
    };
    this.$final = function () {
      this.FList = undefined;
      $lt7.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.FCount;
      return Result;
    };
    this.GetItem = function (Index) {
      var Result = null;
      Result = rtl.getObject(this.FList.Get(Index));
      return Result;
    };
    this.SetItem = function (Index, AObject) {
      var O = null;
      if (this.FFreeObjects) {
        O = rtl.getObject(this.FList.Get(Index));
        this.FList.Put(Index,AObject);
        O = rtl.freeLoc(O);
      } else this.FList.Put(Index,AObject);
    };
    this.Create$1 = function () {
      $lt7.Create.call(this);
      this.FList = $lt8.$create("Create");
      this.FFreeObjects = true;
      return this;
    };
    this.Create$2 = function (FreeObjects) {
      this.Create$1();
      this.FFreeObjects = FreeObjects;
      return this;
    };
    this.Destroy = function () {
      if (this.FList !== null) {
        this.Clear();
        this.FList.$destroy("Destroy");
      };
      $lt7.Destroy.call(this);
    };
    this.Clear = function () {
      var i = 0;
      var O = null;
      if (this.FFreeObjects) for (var $l = this.FList.FCount - 1; $l >= 0; $l--) {
        i = $l;
        O = rtl.getObject(this.FList.Get(i));
        this.FList.Put(i,null);
        O = rtl.freeLoc(O);
      };
      this.FList.Clear();
    };
    this.Add = function (AObject) {
      var Result = 0;
      Result = this.FList.Add(AObject);
      return Result;
    };
  });
  rtl.createClass(this,"THTCustomNode",$lt7,function () {
    $lt1 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FKey = "";
    };
    this.CreateWith = function (AString) {
      $lt7.Create.call(this);
      this.FKey = AString;
      return this;
    };
  });
  rtl.createClass(this,"TFPCustomHashTable",$lt7,function () {
    $lt2 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FHashTable = null;
      this.FHashFunction = null;
      this.FCount = 0;
      this.FHashTableSize = 0;
    };
    this.$final = function () {
      this.FHashTable = undefined;
      this.FHashFunction = undefined;
      $lt7.$final.call(this);
    };
    this.SetHashTableSize = function (Value) {
      var i = 0;
      var newSize = 0;
      if (Value !== this.FHashTableSize) {
        i = 0;
        while (($impl.PRIMELIST[i] < Value) && (i < 27)) i += 1;
        newSize = $impl.PRIMELIST[i];
        if (this.FCount === 0) {
          this.FHashTableSize = newSize;
          this.InitializeHashTable();
        } else this.ChangeTableSize(newSize);
      };
    };
    this.InitializeHashTable = function () {
      var i = 0;
      if (this.FHashTableSize > 0) for (var $l = 0, $end = this.FHashTableSize - 1; $l <= $end; $l++) {
        i = $l;
        this.FHashTable.Add(null);
      };
      this.FCount = 0;
    };
    this.Chain = function (index) {
      var Result = null;
      Result = this.FHashTable.GetItem(index);
      return Result;
    };
    this.FindChainForAdd = function (aKey) {
      var Result = null;
      var hashCode = 0;
      var i = 0;
      hashCode = this.FHashFunction(aKey,this.FHashTableSize);
      Result = this.Chain(hashCode);
      if (Result != null) {
        if (Result.GetCount() > 0) for (var $l = 0, $end = Result.GetCount() - 1; $l <= $end; $l++) {
          i = $l;
          if (Result.GetItem(i).FKey === aKey) throw $lt6.$create("CreateFmt",[rtl.getResStr($mod,"DuplicateMsg"),$lp(18,aKey)]);
        };
      } else {
        this.FHashTable.SetItem(hashCode,$lt.$create("Create$2",[true]));
        Result = this.Chain(hashCode);
      };
      this.FCount += 1;
      return Result;
    };
    this.Create$1 = function () {
      this.CreateWith(196613,$mod.RSHash);
      return this;
    };
    this.CreateWith = function (AHashTableSize, aHashFunc) {
      $lt7.Create.call(this);
      this.FHashTable = $lt.$create("Create$2",[true]);
      this.SetHashTableSize(AHashTableSize);
      this.FHashFunction = aHashFunc;
      return this;
    };
    this.Destroy = function () {
      rtl.free(this,"FHashTable");
      $lt7.Destroy.call(this);
    };
    this.ChangeTableSize = function (ANewSize) {
      var SavedTable = null;
      var List = null;
      var SavedTableSize = 0;
      var i = 0;
      var j = 0;
      var temp = null;
      SavedTable = this.FHashTable;
      SavedTableSize = this.FHashTableSize;
      this.FHashTableSize = ANewSize;
      this.FHashTable = $lt.$create("Create$2",[true]);
      this.InitializeHashTable();
      if (SavedTableSize > 0) for (var $l = 0, $end = SavedTableSize - 1; $l <= $end; $l++) {
        i = $l;
        List = SavedTable.GetItem(i);
        if (List != null) for (var $l1 = 0, $end1 = List.GetCount() - 1; $l1 <= $end1; $l1++) {
          j = $l1;
          temp = List.GetItem(j);
          this.AddNode(temp);
        };
      };
      SavedTable = rtl.freeLoc(SavedTable);
    };
    this.Clear = function () {
      var i = 0;
      if (this.FHashTableSize > 0) for (var $l = 0, $end = this.FHashTableSize - 1; $l <= $end; $l++) {
        i = $l;
        if (this.Chain(i) != null) this.Chain(i).Clear();
      };
      this.FCount = 0;
    };
    this.Find = function (aKey) {
      var Result = null;
      var hashCode = 0;
      var chn = null;
      var i = 0;
      hashCode = this.FHashFunction(aKey,this.FHashTableSize);
      chn = this.Chain(hashCode);
      if (chn != null) if (chn.GetCount() > 0) for (var $l = 0, $end = chn.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        if (chn.GetItem(i).FKey === aKey) return chn.GetItem(i);
      };
      Result = null;
      return Result;
    };
  });
  rtl.createClass(this,"THTObjectNode",$lt1,function () {
    $lt3 = this;
    this.$init = function () {
      $lt1.$init.call(this);
      this.FData = null;
    };
    this.$final = function () {
      this.FData = undefined;
      $lt1.$final.call(this);
    };
  });
  rtl.createClass(this,"THTOwnedObjectNode",$lt3,function () {
    $lt4 = this;
    this.Destroy = function () {
      $lp1({p: this, get: function () {
          return this.p.FData;
        }, set: function (v) {
          this.p.FData = v;
        }});
      $lt7.Destroy.call(this);
    };
  });
  rtl.createClass(this,"TFPObjectHashTable",$lt2,function () {
    $lt5 = this;
    this.$init = function () {
      $lt2.$init.call(this);
      this.FOwnsObjects = false;
    };
    this.CreateNewNode = function (aKey) {
      var Result = null;
      if (this.FOwnsObjects) {
        Result = $lt4.$create("CreateWith",[aKey])}
       else Result = $lt3.$create("CreateWith",[aKey]);
      return Result;
    };
    this.AddNode = function (ANode) {
      this.Add(ANode.FKey,ANode.FData);
    };
    this.GetData = function (index) {
      var Result = null;
      var node = null;
      node = this.Find(index);
      if (node != null) {
        Result = node.FData}
       else Result = null;
      return Result;
    };
    this.Create$2 = function (AOwnsObjects) {
      $lt2.Create$1.call(this);
      this.FOwnsObjects = AOwnsObjects;
      return this;
    };
    this.Add = function (aKey, AItem) {
      var chn = null;
      var NewNode = null;
      chn = this.FindChainForAdd(aKey);
      NewNode = this.CreateNewNode(aKey);
      NewNode.FData = AItem;
      chn.Add(NewNode);
    };
  });
  rtl.createClass(this,"EDuplicate",$lt9,function () {
    $lt6 = this;
  });
  var b = 378551;
  this.RSHash = function (S, TableSize) {
    var Result = 0;
    var a = 0;
    var i = 0;
    a = 63689;
    Result = 0;
    if (S.length > 0) for (var $l = 1, $end = S.length; $l <= $end; $l++) {
      i = $l;
      Result = (Result * a) + S.charCodeAt(i - 1);
      a = a * 378551;
    };
    Result = rtl.lw(Result & 0x7FFFFFFF) % TableSize;
    return Result;
  };
  $mod.$implcode = function () {
    $impl.NPRIMES = 28;
    $impl.PRIMELIST = [53,97,193,389,769,1543,3079,6151,12289,24593,49157,98317,196613,393241,786433,1572869,3145739,6291469,12582917,25165843,50331653,100663319,201326611,402653189,805306457,1610612741,3221225473,4294967291];
    $mod.$resourcestrings = {DuplicateMsg: {org: "An item with key %0:s already exists"}};
  };
},["JS"]);
rtl.module("fpexprpars",["System","Classes","SysUtils","contnrs"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lt15 = null;
  var $lt16 = null;
  var $lt17 = null;
  var $lt18 = null;
  var $lt19 = null;
  var $lt20 = null;
  var $lt21 = null;
  var $lt22 = null;
  var $lt23 = null;
  var $lt24 = null;
  var $lt25 = null;
  var $lt26 = null;
  var $lt27 = null;
  var $lt28 = null;
  var $lt29 = null;
  var $lt30 = null;
  var $lt31 = null;
  var $lt32 = null;
  var $lt33 = null;
  var $lt34 = null;
  var $lt35 = null;
  var $lt36 = null;
  var $lt37 = null;
  var $lt38 = null;
  var $lt39 = null;
  var $lt40 = null;
  var $lt41 = null;
  var $lt42 = null;
  var $lt43 = null;
  var $lt44 = null;
  var $lt45 = null;
  var $lt46 = null;
  var $lt47 = null;
  var $lt48 = null;
  var $lt49 = null;
  var $lt50 = null;
  var $lt51 = null;
  var $lt52 = null;
  var $lt53 = null;
  var $lt54 = null;
  var $lt55 = null;
  var $lt56 = null;
  var $lt57 = null;
  var $lt58 = null;
  var $lt59 = null;
  var $lm = pas.System;
  var $lt60 = $lm.TObject;
  var $lm1 = pas.SysUtils;
  var $lp = $lm1.Format;
  var $lp1 = $lm.VarRecs;
  var $lp2 = $lm1.LowerCase;
  var $lp3 = $lm.upcase;
  var $lp4 = $lm.val$8;
  var $lt61 = $lm1.Exception;
  var $lp5 = $lm1.FreeAndNil;
  var $lp6 = $lm1.TrimLeft;
  var $lp7 = $lm1.IntToStr;
  var $lp8 = $lm1.FormatDateTime;
  var $lm2 = pas.Classes;
  var $lt62 = $lm2.TCollectionItem;
  var $lp9 = $lm1.FloatToStr;
  var $lp10 = $lm1.CurrToStr;
  var $lp11 = $lm1.StrToInt;
  var $lp12 = $lm1.StrToFloat;
  var $lp13 = $lm1.StrToCurr;
  var $lp14 = $lm1.StrToDateTime;
  var $lm3 = null;
  var $lp15 = null;
  var $lt63 = $lm2.TPersistent;
  var $lt64 = $lm2.TCollection;
  var $lp16 = $lm1.CompareText;
  var $lt65 = $lm2.TComponent;
  var $lp17 = $lm1.TryStrToInt64;
  var $lm4 = pas.contnrs;
  var $lt66 = $lm4.TFPObjectHashTable;
  var $lt67 = $lm1.TFormatSettings;
  var $lp18 = $lm.Frac;
  var $lp19 = $lm.Odd;
  var $lp20 = $lm.Sqr$1;
  var $lp21 = $lm.Trunc;
  var $lp22 = $lm.Int;
  var $lp23 = $lm.Copy;
  var $lp24 = $lm.Delete;
  var $lp25 = $lm.Pos;
  var $lp26 = $lm1.UpperCase;
  var $lp27 = $lm1.StringReplace;
  var $lp28 = $lm1.Date;
  var $lp29 = $lm1.Time;
  var $lp30 = $lm1.Now;
  var $lp31 = $lm1.DayOfWeek;
  var $lp32 = $lm1.DecodeDate;
  var $lp33 = $lm1.DecodeTime;
  var $lp34 = $lm1.EncodeDate;
  var $lp35 = $lm1.EncodeTime;
  var $lp36 = $lm1.StrToIntDef$1;
  var $lp37 = $lm1.StrToFloatDef;
  var $lp38 = $lm1.DateToStr;
  var $lp39 = $lm1.TimeToStr;
  var $lp40 = $lm1.StrToDate;
  var $lp41 = $lm1.StrToDateDef;
  var $lp42 = $lm1.StrToTime;
  var $lp43 = $lm1.StrToTimeDef;
  var $lp44 = $lm1.StrToDateTimeDef;
  var $lp45 = $lm1.FormatFloat;
  var $lp46 = $lm1.BoolToStr;
  var $lp47 = $lm1.StrToBool;
  var $lp48 = $lm1.StrToBoolDef;
  $lt = this.TTokenType = {"0": "ttPlus", ttPlus: 0, "1": "ttMinus", ttMinus: 1, "2": "ttLessThan", ttLessThan: 2, "3": "ttLargerThan", ttLargerThan: 3, "4": "ttEqual", ttEqual: 4, "5": "ttDiv", ttDiv: 5, "6": "ttMod", ttMod: 6, "7": "ttMul", ttMul: 7, "8": "ttLeft", ttLeft: 8, "9": "ttRight", ttRight: 9, "10": "ttLessThanEqual", ttLessThanEqual: 10, "11": "ttLargerThanEqual", ttLargerThanEqual: 11, "12": "ttunequal", ttunequal: 12, "13": "ttNumber", ttNumber: 13, "14": "ttString", ttString: 14, "15": "ttIdentifier", ttIdentifier: 15, "16": "ttComma", ttComma: 16, "17": "ttAnd", ttAnd: 17, "18": "ttOr", ttOr: 18, "19": "ttXor", ttXor: 19, "20": "ttTrue", ttTrue: 20, "21": "ttFalse", ttFalse: 21, "22": "ttNot", ttNot: 22, "23": "ttif", ttif: 23, "24": "ttCase", ttCase: 24, "25": "ttPower", ttPower: 25, "26": "ttEOF", ttEOF: 26};
  this.ttComparisons = rtl.createSet(3,2,11,10,4,12);
  this.$rtti.$Class("TFPExpressionParser");
  this.$rtti.$ClassRef("TFPExprFunctionClass",{instancetype: this.$rtti["TFPExprFunction"]});
  $lt1 = this.TNumberKind = {"0": "nkDecimal", nkDecimal: 0, "1": "nkHex", nkHex: 1, "2": "nkOctal", nkOctal: 2, "3": "nkBinary", nkBinary: 3};
  rtl.createClass(this,"TFPExpressionScanner",$lt60,function () {
    $lt2 = this;
    this.$init = function () {
      $lt60.$init.call(this);
      this.FSource = "";
      this.LSource = 0;
      this.FPos = 0;
      this.FChar = "";
      this.FToken = "";
      this.FTokenType = 0;
    };
    this.GetCurrentChar = function () {
      var Result = "";
      Result = this.FChar;
      return Result;
    };
    this.ScanError = function (Msg) {
      throw $lt3.$create("Create$1",[Msg]);
    };
    this.SetSource = function (AValue) {
      this.FSource = AValue;
      this.LSource = this.FSource.length;
      this.FTokenType = 26;
      if (this.LSource === 0) {
        this.FPos = 0;
        this.FChar = $impl.cNull;
      } else {
        this.FPos = 1;
        this.FChar = this.FSource.charAt(0);
      };
      this.FToken = "";
    };
    this.DoIdentifier = function () {
      var Result = 0;
      var C = "";
      var S = "";
      C = this.GetCurrentChar();
      while (!this.IsWordDelim(C) && (C !== $impl.cNull)) {
        if (C !== '"') {
          this.FToken = this.FToken + C}
         else {
          C = this.NextPos();
          while (!(C.charCodeAt() in rtl.createSet($impl.cNull.charCodeAt(),34))) {
            this.FToken = this.FToken + C;
            C = this.NextPos();
          };
          if (C !== '"') this.ScanError($lp(rtl.getResStr($mod,"SErrUnterminatedIdentifier"),$lp1(18,this.FToken)));
        };
        C = this.NextPos();
      };
      S = $lp2(this.FToken);
      if (S === "or") {
        Result = 18}
       else if (S === "xor") {
        Result = 19}
       else if (S === "and") {
        Result = 17}
       else if (S === "true") {
        Result = 20}
       else if (S === "false") {
        Result = 21}
       else if (S === "not") {
        Result = 22}
       else if (S === "if") {
        Result = 23}
       else if (S === "case") {
        Result = 24}
       else if (S === "mod") {
        Result = 6}
       else Result = 15;
      return Result;
    };
    this.DoNumber = function (AKind) {
      var $Self = this;
      var Result = 0;
      var C = "";
      var X = 0.0;
      var I = 0;
      var prevC = "";
      function ValidDigit(C, AKind) {
        var Result = false;
        Result = $Self.IsDigit(C,AKind);
        if (!Result) {
          var $tmp = AKind;
          if ($tmp === 0) {
            Result = (($Self.FToken !== "") && ($lp3(C) === "E")) || (($Self.FToken !== "") && (C.charCodeAt() in rtl.createSet(43,45)) && (prevC === "E"))}
           else if ($tmp === 1) {
            Result = (C === $impl.cHexIdentifier) && (prevC === "\x00")}
           else if ($tmp === 2) {
            Result = (C === $impl.cOctalIdentifier) && (prevC === "\x00")}
           else if ($tmp === 3) Result = (C === $impl.cBinaryIdentifier) && (prevC === "\x00");
        };
        return Result;
      };
      C = this.GetCurrentChar();
      prevC = "\x00";
      while (C !== $impl.cNull) {
        if (this.IsWordDelim(C)) {
          var $tmp = AKind;
          if ($tmp === 0) {
            if (!(prevC.charCodeAt() in rtl.createSet(69,45,43))) break}
           else if (($tmp === 1) || ($tmp === 2)) {
            break}
           else if ($tmp === 3) if (prevC !== "\x00") break;
        };
        if (!ValidDigit(C,AKind)) this.ScanError($lp(rtl.getResStr($mod,"SErrInvalidNumberChar"),$lp1(9,C)));
        this.FToken = this.FToken + C;
        prevC = $lp3(C);
        C = this.NextPos();
      };
      $lp4(this.FToken,{get: function () {
          return X;
        }, set: function (v) {
          X = v;
        }},{get: function () {
          return I;
        }, set: function (v) {
          I = v;
        }});
      if (I !== 0) this.ScanError($lp(rtl.getResStr($mod,"SErrInvalidNumber"),$lp1(18,this.FToken)));
      Result = 13;
      return Result;
    };
    this.DoDelimiter = function () {
      var Result = 0;
      var B = false;
      var C = "";
      var D = "";
      C = this.FChar;
      this.FToken = C;
      B = C.charCodeAt() in rtl.createSet(60,62);
      D = C;
      C = this.NextPos();
      if (B && (C.charCodeAt() in rtl.createSet(61,62))) {
        this.FToken = this.FToken + C;
        this.NextPos();
        if (D === ">") {
          Result = 11}
         else if (C === ">") {
          Result = 12}
         else Result = 10;
      } else {
        var $tmp = D;
        if ($tmp === "+") {
          Result = 0}
         else if ($tmp === "-") {
          Result = 1}
         else if ($tmp === "<") {
          Result = 2}
         else if ($tmp === ">") {
          Result = 3}
         else if ($tmp === "=") {
          Result = 4}
         else if ($tmp === "\/") {
          Result = 5}
         else if ($tmp === "*") {
          Result = 7}
         else if ($tmp === "(") {
          Result = 8}
         else if ($tmp === ")") {
          Result = 9}
         else if ($tmp === ",") {
          Result = 16}
         else if ($tmp === "^") {
          Result = 25}
         else {
          this.ScanError($lp(rtl.getResStr($mod,"SUnknownDelimiter"),$lp1(9,D)));
        };
      };
      return Result;
    };
    this.DoString = function () {
      var $Self = this;
      var Result = 0;
      function TerminatingChar(C) {
        var Result = false;
        Result = (C === $impl.cNull) || ((C === $impl.cSingleQuote) && !(($Self.FPos < $Self.LSource) && ($Self.FSource.charAt(($Self.FPos + 1) - 1) === $impl.cSingleQuote)));
        return Result;
      };
      var C = "";
      this.FToken = "";
      C = this.NextPos();
      while (!TerminatingChar(C)) {
        this.FToken = this.FToken + C;
        if (C === $impl.cSingleQuote) this.NextPos();
        C = this.NextPos();
      };
      if (C === $impl.cNull) this.ScanError(rtl.getResStr($mod,"SBadQuotes"));
      Result = 14;
      this.FTokenType = Result;
      this.NextPos();
      return Result;
    };
    this.NextPos = function () {
      var Result = "";
      this.FPos += 1;
      if (this.FPos > this.FSource.length) {
        this.FChar = $impl.cNull}
       else this.FChar = this.FSource.charAt(this.FPos - 1);
      Result = this.FChar;
      return Result;
    };
    this.SkipWhiteSpace = function () {
      while ((this.FChar.charCodeAt() in $impl.WhiteSpace) && (this.FPos <= this.LSource)) this.NextPos();
    };
    this.IsWordDelim = function (C) {
      var Result = false;
      Result = C.charCodeAt() in $impl.WordDelimiters;
      return Result;
    };
    this.IsDelim = function (C) {
      var Result = false;
      Result = C.charCodeAt() in $impl.Delimiters;
      return Result;
    };
    this.IsDigit = function (C, AKind) {
      var Result = false;
      var $tmp = AKind;
      if ($tmp === 0) {
        Result = C.charCodeAt() in $impl.Digits}
       else if ($tmp === 1) {
        Result = C.charCodeAt() in $impl.HexDigits}
       else if ($tmp === 2) {
        Result = C.charCodeAt() in $impl.OctalDigits}
       else if ($tmp === 3) Result = C.charCodeAt() in $impl.BinaryDigits;
      return Result;
    };
    this.IsAlpha = function (C) {
      var Result = false;
      Result = C.charCodeAt() in rtl.createSet(null,65,90,null,97,122);
      return Result;
    };
    this.Create$1 = function () {
      this.SetSource("");
      return this;
    };
    this.GetToken = function () {
      var Result = 0;
      var C = "";
      this.FToken = "";
      this.SkipWhiteSpace();
      C = this.FChar;
      if (C === $impl.cNull) {
        Result = 26}
       else if (this.IsDelim(C)) {
        Result = this.DoDelimiter()}
       else if (C === $impl.cSingleQuote) {
        Result = this.DoString()}
       else if (C === $impl.cHexIdentifier) {
        Result = this.DoNumber(1)}
       else if (C === $impl.cOctalIdentifier) {
        Result = this.DoNumber(2)}
       else if (C === $impl.cBinaryIdentifier) {
        Result = this.DoNumber(3)}
       else if (this.IsDigit(C,0)) {
        Result = this.DoNumber(0)}
       else if (this.IsAlpha(C) || (C === '"') || (C === "_")) {
        Result = this.DoIdentifier()}
       else this.ScanError($lp(rtl.getResStr($mod,"SErrUnknownCharacter"),$lp1(0,this.FPos,9,C)));
      this.FTokenType = Result;
      return Result;
    };
  });
  rtl.createClass(this,"EExprScanner",$lt61,function () {
    $lt3 = this;
  });
  $lt4 = this.TResultType = {"0": "rtBoolean", rtBoolean: 0, "1": "rtInteger", rtInteger: 1, "2": "rtFloat", rtFloat: 2, "3": "rtDateTime", rtDateTime: 3, "4": "rtString", rtString: 4, "5": "rtCurrency", rtCurrency: 5};
  this.$rtti.$Enum("TResultType",{minvalue: 0, maxvalue: 5, ordtype: 1, enumtype: this.TResultType});
  rtl.recNewT(this,"TFPExpressionResult",function () {
    $lt5 = this;
    this.ResultType = 0;
    this.resValue = undefined;
    this.$eq = function (b) {
      return (this.ResultType === b.ResultType) && (this.resValue === b.resValue);
    };
    this.$assign = function (s) {
      this.ResultType = s.ResultType;
      this.resValue = s.resValue;
      return this;
    };
    var $r = $mod.$rtti.$Record("TFPExpressionResult",{});
    $r.addField("ResultType",$mod.$rtti["TResultType"]);
    $r.addField("resValue",rtl.jsvalue);
  });
  this.$rtti.$DynArray("TExprParameterArray",{eltype: this.$rtti["TFPExpressionResult"]});
  rtl.createClass(this,"TFPExprNode",$lt60,function () {
    $lt6 = this;
    this.CheckNodeType = function (Anode, Allowed) {
      var S = "";
      var A = 0;
      if (Anode === null) $impl.RaiseParserError(rtl.getResStr($mod,"SErrNoNodeToCheck"));
      if (!(Anode.NodeType() in Allowed)) {
        S = "";
        for (A = 0; A <= 5; A++) if (A in Allowed) {
          if (S !== "") S = S + ",";
          S = S + $mod.ResultTypeName(A);
        };
        $impl.RaiseParserError$1(rtl.getResStr($mod,"SInvalidNodeType"),$lp1(18,$mod.ResultTypeName(Anode.NodeType()),18,S,18,Anode.AsString()));
      };
    };
  });
  rtl.createClass(this,"TFPBinaryOperation",$lt6,function () {
    $lt7 = this;
    this.$init = function () {
      $lt6.$init.call(this);
      this.FLeft = null;
      this.FRight = null;
    };
    this.$final = function () {
      this.FLeft = undefined;
      this.FRight = undefined;
      $lt6.$final.call(this);
    };
    this.CheckSameNodeTypes = function () {
      var LT = 0;
      var RT = 0;
      LT = this.FLeft.NodeType();
      RT = this.FRight.NodeType();
      if (RT !== LT) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrTypesDoNotMatch"),$lp1(18,$mod.ResultTypeName(LT),18,$mod.ResultTypeName(RT),18,this.FLeft.AsString(),18,this.FRight.AsString()));
    };
    this.Create$1 = function (ALeft, ARight) {
      this.FLeft = ALeft;
      this.FRight = ARight;
      return this;
    };
    this.Destroy = function () {
      $lp5({p: this, get: function () {
          return this.p.FLeft;
        }, set: function (v) {
          this.p.FLeft = v;
        }});
      $lp5({p: this, get: function () {
          return this.p.FRight;
        }, set: function (v) {
          this.p.FRight = v;
        }});
      $lt60.Destroy.call(this);
    };
    this.Check = function () {
      if (!(this.FLeft != null)) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrNoleftOperand"),$lp1(18,this.$classname));
      if (!(this.FRight != null)) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrNoRightOperand"),$lp1(18,this.$classname));
    };
  });
  rtl.createClass(this,"TFPBooleanOperation",$lt7,function () {
    $lt8 = this;
    this.Check = function () {
      $lt7.Check.call(this);
      this.CheckNodeType(this.FLeft,rtl.createSet(1,0));
      this.CheckNodeType(this.FRight,rtl.createSet(1,0));
      this.CheckSameNodeTypes();
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FLeft.NodeType();
      return Result;
    };
  });
  rtl.createClass(this,"TFPBinaryAndOperation",$lt8,function () {
    $lt9 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 0) {
        Result.resValue = !(Result.resValue == false) && !(RRes.resValue == false)}
       else if ($tmp === 1) Result.resValue = rtl.and(rtl.trunc(Result.resValue),rtl.trunc(RRes.resValue));
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " and " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPBinaryOrOperation",$lt8,function () {
    $lt10 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 0) {
        Result.resValue = !(Result.resValue == false) || !(RRes.resValue == false)}
       else if ($tmp === 1) Result.resValue = rtl.or(rtl.trunc(Result.resValue),rtl.trunc(RRes.resValue));
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " or " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPBinaryXOrOperation",$lt8,function () {
    $lt11 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 0) {
        Result.resValue = !(Result.resValue == false) ^ !(RRes.resValue == false)}
       else if ($tmp === 1) Result.resValue = rtl.xor(rtl.trunc(Result.resValue),rtl.trunc(RRes.resValue));
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " xor " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPBooleanResultOperation",$lt7,function () {
    $lt12 = this;
    this.Check = function () {
      $lt7.Check.call(this);
      this.CheckSameNodeTypes();
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 0;
      return Result;
    };
  });
  rtl.createClass(this,"TFPEqualOperation",$lt12,function () {
    $lt13 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      Result.resValue = Result.resValue == RRes.resValue;
      Result.ResultType = 0;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " = " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPUnequalOperation",$lt13,function () {
    $lt14 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign($lt13.GetNodeValue.call(this));
      Result.resValue = !Result.resValue;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " <> " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPOrderingOperation",$lt12,function () {
    $lt15 = this;
    var AllowedTypes = rtl.createSet(1,2,5,3,4);
    this.Check = function () {
      this.CheckNodeType(this.FLeft,AllowedTypes);
      this.CheckNodeType(this.FRight,AllowedTypes);
      $lt12.Check.call(this);
    };
  });
  rtl.createClass(this,"TFPLessThanOperation",$lt15,function () {
    $lt16 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = rtl.trunc(Result.resValue) < rtl.trunc(RRes.resValue)}
       else if ($tmp === 2) {
        Result.resValue = rtl.getNumber(Result.resValue) < rtl.getNumber(RRes.resValue)}
       else if ($tmp === 5) {
        Result.resValue = rtl.trunc(Result.resValue * 10000) < rtl.trunc(RRes.resValue * 10000)}
       else if ($tmp === 3) {
        Result.resValue = rtl.getNumber(Result.resValue) < rtl.getNumber(RRes.resValue)}
       else if ($tmp === 4) Result.resValue = ("" + Result.resValue) < ("" + RRes.resValue);
      Result.ResultType = 0;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " < " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPGreaterThanOperation",$lt15,function () {
    $lt17 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        var $tmp1 = this.FRight.NodeType();
        if ($tmp1 === 1) {
          Result.resValue = rtl.trunc(Result.resValue) > rtl.trunc(RRes.resValue)}
         else if ($tmp1 === 2) {
          Result.resValue = rtl.trunc(Result.resValue) > rtl.getNumber(RRes.resValue)}
         else if ($tmp1 === 5) Result.resValue = (rtl.trunc(Result.resValue) * 10000) > rtl.trunc(RRes.resValue * 10000);
      } else if ($tmp === 2) {
        var $tmp2 = this.FRight.NodeType();
        if ($tmp2 === 1) {
          Result.resValue = rtl.getNumber(Result.resValue) > rtl.trunc(RRes.resValue)}
         else if ($tmp2 === 2) {
          Result.resValue = rtl.getNumber(Result.resValue) > rtl.getNumber(RRes.resValue)}
         else if ($tmp2 === 5) Result.resValue = (rtl.getNumber(Result.resValue) * 10000) > rtl.trunc(RRes.resValue * 10000);
      } else if ($tmp === 5) {
        var $tmp3 = this.FRight.NodeType();
        if ($tmp3 === 1) {
          Result.resValue = rtl.trunc(Result.resValue * 10000) > (rtl.trunc(RRes.resValue) * 10000)}
         else if ($tmp3 === 2) {
          Result.resValue = rtl.trunc(Result.resValue * 10000) > (rtl.getNumber(RRes.resValue) * 10000)}
         else if ($tmp3 === 5) Result.resValue = rtl.trunc(Result.resValue * 10000) > rtl.trunc(RRes.resValue * 10000);
      } else if ($tmp === 3) {
        Result.resValue = rtl.getNumber(Result.resValue) > rtl.getNumber(RRes.resValue)}
       else if ($tmp === 4) Result.resValue = ("" + Result.resValue) > ("" + RRes.resValue);
      Result.ResultType = 0;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " > " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPLessThanEqualOperation",$lt17,function () {
    $lt18 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign($lt17.GetNodeValue.call(this));
      Result.resValue = !Result.resValue;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " <= " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPGreaterThanEqualOperation",$lt16,function () {
    $lt19 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign($lt16.GetNodeValue.call(this));
      Result.resValue = !Result.resValue;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " >= " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TIfOperation",$lt7,function () {
    $lt20 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FCondition = null;
    };
    this.$final = function () {
      this.FCondition = undefined;
      $lt7.$final.call(this);
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FCondition.GetNodeValue());
      if (!(Result.resValue == false)) {
        Result.$assign(this.FLeft.GetNodeValue())}
       else Result.$assign(this.FRight.GetNodeValue());
      return Result;
    };
    this.Check = function () {
      $lt7.Check.call(this);
      if (this.FCondition.NodeType() !== 0) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrIFNeedsBoolean"),$lp1(18,this.FCondition.AsString()));
      this.CheckSameNodeTypes();
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FLeft.NodeType();
      return Result;
    };
    this.Create$2 = function (ACondition, ALeft, ARight) {
      $lt7.Create$1.call(this,ALeft,ARight);
      this.FCondition = ACondition;
      return this;
    };
    this.Destroy = function () {
      $lp5({p: this, get: function () {
          return this.p.FCondition;
        }, set: function (v) {
          this.p.FCondition = v;
        }});
      $lt7.Destroy.call(this);
    };
    this.AsString = function () {
      var Result = "";
      Result = $lp("if(%s , %s , %s)",$lp1(18,this.FCondition.AsString(),18,this.FLeft.AsString(),18,this.FRight.AsString()));
      return Result;
    };
  });
  rtl.createClass(this,"TCaseOperation",$lt6,function () {
    $lt21 = this;
    this.$init = function () {
      $lt6.$init.call(this);
      this.FArgs = [];
    };
    this.$final = function () {
      this.FArgs = undefined;
      $lt6.$final.call(this);
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var I = 0;
      var L = 0;
      var B = false;
      var RT = $lt5.$new();
      var RV = $lt5.$new();
      RT.$assign(this.FArgs[0].GetNodeValue());
      L = rtl.length(this.FArgs);
      I = 2;
      B = false;
      while (!B && (I < L)) {
        Result.$assign(this.FArgs[I].GetNodeValue());
        var $tmp = RT.ResultType;
        if ($tmp === 0) {
          B = RT.resValue == RV.resValue}
         else if ($tmp === 1) {
          B = RT.resValue == RV.resValue}
         else if ($tmp === 2) {
          B = RT.resValue == RV.resValue}
         else if ($tmp === 5) {
          B = RT.resValue == RV.resValue}
         else if ($tmp === 3) {
          B = RT.resValue == RV.resValue}
         else if ($tmp === 4) B = RT.resValue == RV.resValue;
        if (!B) I += 2;
      };
      Result.ResultType = this.FArgs[1].NodeType();
      if (B) {
        Result.$assign(this.FArgs[I + 1].GetNodeValue())}
       else if ((L % 2) === 0) Result.$assign(this.FArgs[1].GetNodeValue());
      return Result;
    };
    this.Check = function () {
      var T = 0;
      var V = 0;
      var I = 0;
      var N = null;
      if (rtl.length(this.FArgs) < 3) $impl.RaiseParserError(rtl.getResStr($mod,"SErrCaseNeeds3"));
      if ((rtl.length(this.FArgs) % 2) === 1) $impl.RaiseParserError(rtl.getResStr($mod,"SErrCaseEvenCount"));
      T = this.FArgs[0].NodeType();
      V = this.FArgs[1].NodeType();
      for (var $l = 2, $end = rtl.length(this.FArgs) - 1; $l <= $end; $l++) {
        I = $l;
        N = this.FArgs[I];
        if ((I % 2) === 0) {
          if (!$lt41.isPrototypeOf(N)) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrCaseLabelNotAConst"),$lp1(0,rtl.trunc(I / 2),18,N.AsString()));
          if (N.NodeType() !== T) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrCaseLabelType"),$lp1(0,rtl.trunc(I / 2),18,N.AsString(),18,$mod.ResultTypeName(T),18,$mod.ResultTypeName(N.NodeType())));
        } else {
          if (N.NodeType() !== V) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrCaseValueType"),$lp1(0,rtl.trunc((I - 1) / 2),18,N.AsString(),18,$mod.ResultTypeName(V),18,$mod.ResultTypeName(N.NodeType())));
        };
      };
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FArgs[1].NodeType();
      return Result;
    };
    this.Create$1 = function (Args) {
      this.FArgs = rtl.arrayRef(Args);
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      for (var $l = 0, $end = rtl.length(this.FArgs) - 1; $l <= $end; $l++) {
        I = $l;
        $lp5({a: I, p: this.FArgs, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
      };
      $lt60.Destroy.call(this);
    };
    this.AsString = function () {
      var Result = "";
      var I = 0;
      Result = "";
      for (var $l = 0, $end = rtl.length(this.FArgs) - 1; $l <= $end; $l++) {
        I = $l;
        if (Result !== "") Result = Result + ", ";
        Result = Result + this.FArgs[I].AsString();
      };
      Result = "Case(" + Result + ")";
      return Result;
    };
  });
  rtl.createClass(this,"TMathOperation",$lt7,function () {
    $lt22 = this;
    var AllowedTypes = rtl.createSet(1,2,5,3,4);
    this.Check = function () {
      $lt7.Check.call(this);
      this.CheckNodeType(this.FLeft,AllowedTypes);
      this.CheckNodeType(this.FRight,AllowedTypes);
      this.CheckSameNodeTypes();
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FLeft.NodeType();
      return Result;
    };
  });
  rtl.createClass(this,"TFPAddOperation",$lt22,function () {
    $lt23 = this;
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = rtl.trunc(Result.resValue) + rtl.trunc(RRes.resValue)}
       else if ($tmp === 4) {
        Result.resValue = "" + Result.resValue + ("" + RRes.resValue)}
       else if ($tmp === 3) {
        Result.resValue = rtl.getNumber(Result.resValue) + rtl.getNumber(RRes.resValue)}
       else if ($tmp === 2) {
        Result.resValue = rtl.getNumber(Result.resValue) + rtl.getNumber(RRes.resValue)}
       else if ($tmp === 5) Result.resValue = (rtl.trunc(Result.resValue * 10000) + rtl.trunc(RRes.resValue * 10000)) / 10000;
      Result.ResultType = this.NodeType();
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " + " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPSubtractOperation",$lt22,function () {
    $lt24 = this;
    var AllowedTypes$1 = rtl.createSet(1,2,5,3);
    this.Check = function () {
      this.CheckNodeType(this.FLeft,AllowedTypes$1);
      this.CheckNodeType(this.FRight,AllowedTypes$1);
      $lt22.Check.call(this);
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = rtl.trunc(Result.resValue) - rtl.trunc(RRes.resValue)}
       else if ($tmp === 3) {
        Result.resValue = rtl.getNumber(Result.resValue) - rtl.getNumber(RRes.resValue)}
       else if ($tmp === 2) {
        Result.resValue = rtl.getNumber(Result.resValue) - rtl.getNumber(RRes.resValue)}
       else if ($tmp === 5) Result.resValue = (rtl.trunc(Result.resValue * 10000) - rtl.trunc(RRes.resValue * 10000)) / 10000;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " - " + this.FRight.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPMultiplyOperation",$lt22,function () {
    $lt25 = this;
    var AllowedTypes$1 = rtl.createSet(1,5,2);
    this.Check = function () {
      this.CheckNodeType(this.FLeft,AllowedTypes$1);
      this.CheckNodeType(this.FRight,AllowedTypes$1);
      $lt22.Check.call(this);
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " * " + this.FRight.AsString();
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = rtl.trunc(Result.resValue) * rtl.trunc(RRes.resValue)}
       else if ($tmp === 2) {
        Result.resValue = rtl.getNumber(Result.resValue) * rtl.getNumber(RRes.resValue)}
       else if ($tmp === 5) Result.resValue = ((rtl.trunc(Result.resValue * 10000) * rtl.trunc(RRes.resValue * 10000)) / 10000) / 10000;
      return Result;
    };
  });
  rtl.createClass(this,"TFPDivideOperation",$lt22,function () {
    $lt26 = this;
    var AllowedTypes$1 = rtl.createSet(1,5,2);
    this.Check = function () {
      this.CheckNodeType(this.FLeft,AllowedTypes$1);
      this.CheckNodeType(this.FRight,AllowedTypes$1);
      $lt22.Check.call(this);
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " \/ " + this.FRight.AsString();
      return Result;
    };
    this.NodeType = function () {
      var Result = 0;
      if ((this.FLeft.NodeType() === 5) && (this.FRight.NodeType() === 5)) {
        Result = 5}
       else Result = 2;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = rtl.trunc(Result.resValue) / rtl.trunc(RRes.resValue)}
       else if ($tmp === 2) {
        Result.resValue = rtl.getNumber(Result.resValue) / rtl.getNumber(RRes.resValue)}
       else if ($tmp === 5) if (this.NodeType() === 5) {
        Result.resValue = rtl.trunc((rtl.trunc(Result.resValue * 10000) / rtl.trunc(RRes.resValue * 10000)) * 10000) / 10000}
       else Result.resValue = rtl.getNumber(Result.resValue) / rtl.getNumber(RRes.resValue);
      Result.ResultType = this.NodeType();
      return Result;
    };
  });
  rtl.createClass(this,"TFPModuloOperation",$lt22,function () {
    $lt27 = this;
    this.Check = function () {
      this.CheckNodeType(this.FLeft,rtl.createSet(1));
      this.CheckNodeType(this.FRight,rtl.createSet(1));
      $lt22.Check.call(this);
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + " mod " + this.FRight.AsString();
      return Result;
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 1;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      Result.resValue = rtl.trunc(Result.resValue) % rtl.trunc(RRes.resValue);
      Result.ResultType = 1;
      return Result;
    };
  });
  rtl.createClass(this,"TFPPowerOperation",$lt22,function () {
    $lt28 = this;
    var AllowedTypes$1 = rtl.createSet(1,5,2);
    this.Check = function () {
      this.CheckNodeType(this.FLeft,AllowedTypes$1);
      this.CheckNodeType(this.FRight,AllowedTypes$1);
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FLeft.AsString() + "^" + this.FRight.AsString();
      return Result;
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 2;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var RRes = $lt5.$new();
      Result.$assign(this.FLeft.GetNodeValue());
      RRes.$assign(this.FRight.GetNodeValue());
      Result.resValue = $impl.power($mod.ArgToFloat($lt5.$clone(Result)),$mod.ArgToFloat($lt5.$clone(RRes)));
      Result.ResultType = 2;
      return Result;
    };
  });
  rtl.createClass(this,"TFPUnaryOperator",$lt6,function () {
    $lt29 = this;
    this.$init = function () {
      $lt6.$init.call(this);
      this.FOperand = null;
    };
    this.$final = function () {
      this.FOperand = undefined;
      $lt6.$final.call(this);
    };
    this.Create$1 = function (AOperand) {
      this.FOperand = AOperand;
      return this;
    };
    this.Destroy = function () {
      $lp5({p: this, get: function () {
          return this.p.FOperand;
        }, set: function (v) {
          this.p.FOperand = v;
        }});
      $lt60.Destroy.call(this);
    };
    this.Check = function () {
      if (!(this.FOperand != null)) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrNoOperand"),$lp1(18,this.$classname));
    };
  });
  rtl.createClass(this,"TFPConvertNode",$lt29,function () {
    $lt30 = this;
    this.AsString = function () {
      var Result = "";
      Result = this.FOperand.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TFPNotNode",$lt29,function () {
    $lt31 = this;
    this.Check = function () {
      if (!(this.FOperand.NodeType() in rtl.createSet(1,0))) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrNoNOTOperation"),$lp1(18,$mod.ResultTypeName(this.FOperand.NodeType()),18,this.FOperand.AsString()));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FOperand.NodeType();
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = !Result.resValue}
       else if ($tmp === 0) Result.resValue = !Result.resValue;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = "not " + this.FOperand.AsString();
      return Result;
    };
  });
  rtl.createClass(this,"TIntConvertNode",$lt30,function () {
    $lt32 = this;
    this.Check = function () {
      $lt29.Check.call(this);
      this.CheckNodeType(this.FOperand,rtl.createSet(1));
    };
  });
  rtl.createClass(this,"TIntToFloatNode",$lt32,function () {
    $lt33 = this;
    this.NodeType = function () {
      var Result = 0;
      Result = 2;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.resValue = Result.resValue;
      Result.ResultType = 2;
      return Result;
    };
  });
  rtl.createClass(this,"TIntToCurrencyNode",$lt32,function () {
    $lt34 = this;
    this.NodeType = function () {
      var Result = 0;
      Result = 5;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.resValue = Result.resValue;
      Result.ResultType = 5;
      return Result;
    };
  });
  rtl.createClass(this,"TIntToDateTimeNode",$lt32,function () {
    $lt35 = this;
    this.NodeType = function () {
      var Result = 0;
      Result = 3;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.ResultType = 3;
      return Result;
    };
  });
  rtl.createClass(this,"TFloatToDateTimeNode",$lt30,function () {
    $lt36 = this;
    this.Check = function () {
      $lt29.Check.call(this);
      this.CheckNodeType(this.FOperand,rtl.createSet(2));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 3;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.ResultType = 3;
      return Result;
    };
  });
  rtl.createClass(this,"TFloatToCurrencyNode",$lt30,function () {
    $lt37 = this;
    this.Check = function () {
      this.CheckNodeType(this.FOperand,rtl.createSet(2));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 5;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.ResultType = 5;
      Result.resValue = Result.resValue;
      return Result;
    };
  });
  rtl.createClass(this,"TCurrencyToDateTimeNode",$lt30,function () {
    $lt38 = this;
    this.Check = function () {
      $lt29.Check.call(this);
      this.CheckNodeType(this.FOperand,rtl.createSet(5));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 3;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      var R = $lt5.$new();
      R.$assign(this.FOperand.GetNodeValue());
      Result.resValue = rtl.trunc(R.resValue * 10000) / 10000;
      Result.ResultType = 3;
      return Result;
    };
  });
  rtl.createClass(this,"TCurrencyToFloatNode",$lt30,function () {
    $lt39 = this;
    this.Check = function () {
      $lt29.Check.call(this);
      this.CheckNodeType(this.FOperand,rtl.createSet(5));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = 2;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      Result.resValue = rtl.trunc(Result.resValue * 10000) / 10000;
      Result.ResultType = 2;
      return Result;
    };
  });
  rtl.createClass(this,"TFPNegateOperation",$lt29,function () {
    $lt40 = this;
    this.Check = function () {
      $lt29.Check.call(this);
      if (!(this.FOperand.NodeType() in rtl.createSet(1,2,5))) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrNoNegation"),$lp1(18,$mod.ResultTypeName(this.FOperand.NodeType()),18,this.FOperand.AsString()));
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FOperand.NodeType();
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FOperand.GetNodeValue());
      var $tmp = Result.ResultType;
      if ($tmp === 1) {
        Result.resValue = -rtl.trunc(Result.resValue)}
       else if ($tmp === 2) {
        Result.resValue = -rtl.getNumber(Result.resValue)}
       else if ($tmp === 5) Result.resValue = -rtl.trunc(Result.resValue * 10000) / 10000;
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      Result = "-" + $lp6(this.FOperand.AsString());
      return Result;
    };
  });
  rtl.createClass(this,"TFPConstExpression",$lt6,function () {
    $lt41 = this;
    this.$init = function () {
      $lt6.$init.call(this);
      this.FValue = $lt5.$new();
    };
    this.$final = function () {
      this.FValue = undefined;
      $lt6.$final.call(this);
    };
    this.CreateString = function (AValue) {
      this.FValue.ResultType = 4;
      this.FValue.resValue = AValue;
      return this;
    };
    this.CreateInteger = function (AValue) {
      this.FValue.ResultType = 1;
      this.FValue.resValue = AValue;
      return this;
    };
    this.CreateFloat = function (AValue) {
      $lt60.Create.call(this);
      this.FValue.ResultType = 2;
      this.FValue.resValue = AValue;
      return this;
    };
    this.CreateBoolean = function (AValue) {
      this.FValue.ResultType = 0;
      this.FValue.resValue = AValue;
      return this;
    };
    this.Check = function () {
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FValue.ResultType;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FValue);
      return Result;
    };
    this.AsString = function () {
      var Result = "";
      var $tmp = this.NodeType();
      if ($tmp === 4) {
        Result = "'" + ("" + this.FValue.resValue) + "'"}
       else if ($tmp === 1) {
        Result = $lp7(rtl.trunc(this.FValue.resValue))}
       else if ($tmp === 3) {
        Result = "'" + $lp8("cccc",rtl.getNumber(this.FValue.resValue)) + "'"}
       else if ($tmp === 0) {
        if (!(this.FValue.resValue == false)) {
          Result = "True"}
         else Result = "False"}
       else if ($tmp === 2) {
        Result = rtl.floatToStr(rtl.getNumber(this.FValue.resValue))}
       else if ($tmp === 5) Result = rtl.floatToStr(rtl.trunc(this.FValue.resValue * 10000) / 10000);
      return Result;
    };
  });
  $lt42 = this.TIdentifierType = {"0": "itVariable", itVariable: 0, "1": "itFunctionHandler", itFunctionHandler: 1, "2": "itFunctionNode", itFunctionNode: 2};
  this.$rtti.$Enum("TIdentifierType",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TIdentifierType});
  this.$rtti.$RefToProcVar("TFPExprFunctionEvent",{procsig: rtl.newTIProcSig([["Args",this.$rtti["TExprParameterArray"],2]],this.$rtti["TFPExpressionResult"])});
  this.$rtti.$RefToProcVar("TFPExprVariableEvent",{procsig: rtl.newTIProcSig([["AName",rtl.string,2]],this.$rtti["TFPExpressionResult"])});
  rtl.createClass(this,"TFPExprIdentifierDef",$lt62,function () {
    $lt43 = this;
    this.$init = function () {
      $lt62.$init.call(this);
      this.FNodeType = null;
      this.FOnGetVarValue = null;
      this.FStringValue = "";
      this.FValue = $lt5.$new();
      this.FArgumentTypes = "";
      this.FIDType = 0;
      this.FName = "";
      this.FOnGetValue = null;
    };
    this.$final = function () {
      this.FNodeType = undefined;
      this.FOnGetVarValue = undefined;
      this.FValue = undefined;
      this.FOnGetValue = undefined;
      $lt62.$final.call(this);
    };
    this.GetResultType = function () {
      var Result = 0;
      Result = this.FValue.ResultType;
      return Result;
    };
    this.GetValue = function () {
      var Result = "";
      var $tmp = this.FValue.ResultType;
      if ($tmp === 0) {
        if (this.FValue.resValue) {
          Result = "True"}
         else Result = "False"}
       else if ($tmp === 1) {
        Result = $lp7(rtl.trunc(this.FValue.resValue))}
       else if ($tmp === 2) {
        Result = $lp9(rtl.getNumber(this.FValue.resValue))}
       else if ($tmp === 5) {
        Result = $lp10(rtl.trunc(this.FValue.resValue * 10000))}
       else if ($tmp === 3) {
        Result = $lp8("cccc",rtl.getNumber(this.FValue.resValue))}
       else if ($tmp === 4) Result = "" + this.FValue.resValue;
      return Result;
    };
    this.SetArgumentTypes = function (AValue) {
      var I = 0;
      if (this.FArgumentTypes === AValue) return;
      for (var $l = 1, $end = AValue.length; $l <= $end; $l++) {
        I = $l;
        $mod.CharToResultType(AValue.charAt(I - 1));
      };
      this.FArgumentTypes = AValue;
    };
    this.SetName = function (AValue) {
      if (this.FName === AValue) return;
      if (AValue !== "") if ((this.FCollection != null) && (this.FCollection.IndexOfIdentifier(AValue) !== -1)) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrDuplicateIdentifier"),$lp1(18,AValue));
      this.FName = AValue;
    };
    this.SetResultType = function (AValue) {
      if (AValue !== this.FValue.ResultType) {
        this.FValue.ResultType = AValue;
        this.SetValue(this.FStringValue);
      };
    };
    this.SetValue = function (AValue) {
      this.FStringValue = AValue;
      if (AValue !== "") {
        var $tmp = this.FValue.ResultType;
        if ($tmp === 0) {
          this.FValue.resValue = this.FStringValue === "True"}
         else if ($tmp === 1) {
          this.FValue.resValue = $lp11(AValue)}
         else if ($tmp === 2) {
          this.FValue.resValue = $lp12(AValue)}
         else if ($tmp === 5) {
          this.FValue.resValue = $lp13(AValue) / 10000}
         else if ($tmp === 3) {
          this.FValue.resValue = $lp14(AValue)}
         else if ($tmp === 4) this.FValue.resValue = AValue;
      } else {
        var $tmp1 = this.FValue.ResultType;
        if ($tmp1 === 0) {
          this.FValue.resValue = false}
         else if ($tmp1 === 1) {
          this.FValue.resValue = 0}
         else if ($tmp1 === 2) {
          this.FValue.resValue = 0.0}
         else if ($tmp1 === 5) {
          this.FValue.resValue = 0.0}
         else if ($tmp1 === 3) {
          this.FValue.resValue = 0}
         else if ($tmp1 === 4) this.FValue.resValue = "";
      };
    };
    this.FetchValue = function () {
      var RT = 0;
      var RT2 = 0;
      var I = 0;
      RT = this.GetResultType();
      if (this.FOnGetVarValue != null) this.FValue.$assign(this.FOnGetVarValue(this.FName));
      RT2 = this.FValue.ResultType;
      if (RT2 !== RT) {
        if ((RT2 === 1) && (RT === 2)) {
          this.FValue.ResultType = RT;
          I = rtl.trunc(this.FValue.resValue);
          this.FValue.resValue = I;
        } else {
          this.FValue.ResultType = RT;
          throw $lt59.$create("CreateFmt",['Value handler for variable %s returned wrong type, expected "%s", got "%s"',$lp1(18,this.FName,18,$lp15($mod.$rtti["TResultType"],RT),18,$lp15($mod.$rtti["TResultType"],RT2))]);
        };
      };
    };
    this.ArgumentCount = function () {
      var Result = 0;
      Result = this.FArgumentTypes.length;
      return Result;
    };
    this.Assign = function (Source) {
      var EID = null;
      if ($lt43.isPrototypeOf(Source)) {
        EID = rtl.as(Source,$lt43);
        this.FStringValue = EID.FStringValue;
        this.FValue.$assign(EID.FValue);
        this.FArgumentTypes = EID.FArgumentTypes;
        this.FIDType = EID.FIDType;
        this.FName = EID.FName;
        this.FOnGetValue = EID.FOnGetValue;
        this.FOnGetVarValue = EID.FOnGetVarValue;
      } else $lt63.Assign.call(this,Source);
    };
    this.EventBasedVariable = function () {
      var Result = false;
      Result = this.FOnGetVarValue != null;
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("IdentifierType",0,$mod.$rtti["TIdentifierType"],"FIDType","FIDType");
    $r.addProperty("Name",2,rtl.string,"FName","SetName");
    $r.addProperty("Value",3,rtl.string,"GetValue","SetValue");
    $r.addProperty("ParameterTypes",2,rtl.string,"FArgumentTypes","SetArgumentTypes");
    $r.addProperty("ResultType",3,$mod.$rtti["TResultType"],"GetResultType","SetResultType");
    $r.addProperty("OnGetFunctionValue",0,$mod.$rtti["TFPExprFunctionEvent"],"FOnGetValue","FOnGetValue");
    $r.addProperty("OnGetVariableValue",0,$mod.$rtti["TFPExprVariableEvent"],"FOnGetVarValue","FOnGetVarValue");
    $r.addProperty("NodeType",0,$mod.$rtti["TFPExprFunctionClass"],"FNodeType","FNodeType");
  });
  $lt44 = this.TBuiltInCategory = {"0": "bcStrings", bcStrings: 0, "1": "bcDateTime", bcDateTime: 1, "2": "bcMath", bcMath: 2, "3": "bcBoolean", bcBoolean: 3, "4": "bcConversion", bcConversion: 4, "5": "bcData", bcData: 5, "6": "bcVaria", bcVaria: 6, "7": "bcUser", bcUser: 7, "8": "bcAggregate", bcAggregate: 8};
  this.$rtti.$Enum("TBuiltInCategory",{minvalue: 0, maxvalue: 8, ordtype: 1, enumtype: this.TBuiltInCategory});
  this.$rtti.$Set("TBuiltInCategories",{comptype: this.$rtti["TBuiltInCategory"]});
  rtl.createClass(this,"TFPBuiltInExprIdentifierDef",$lt43,function () {
    $lt45 = this;
    this.$init = function () {
      $lt43.$init.call(this);
      this.FCategory = 0;
    };
    this.Assign = function (Source) {
      $lt43.Assign.call(this,Source);
      if ($lt45.isPrototypeOf(Source)) this.FCategory = rtl.as(Source,$lt45).FCategory;
    };
    var $r = this.$rtti;
    $r.addProperty("Category",0,$mod.$rtti["TBuiltInCategory"],"FCategory","FCategory");
  });
  rtl.createClass(this,"TFPExprIdentifierDefs",$lt64,function () {
    $lt46 = this;
    this.$init = function () {
      $lt64.$init.call(this);
      this.FParser = null;
    };
    this.$final = function () {
      this.FParser = undefined;
      $lt64.$final.call(this);
    };
    this.GetI = function (AIndex) {
      var Result = null;
      Result = this.GetItem(AIndex);
      return Result;
    };
    this.Update = function (Item) {
      $lt64.Update.call(this,Item);
      if (this.FParser != null) this.FParser.FDirty = true;
    };
    this.IndexOfIdentifier = function (AName) {
      var Result = 0;
      Result = this.GetCount() - 1;
      while ((Result >= 0) && ($lp16(this.GetI(Result).FName,AName) !== 0)) Result -= 1;
      return Result;
    };
    this.AddVariable = function (AName, AResultType, ACallback) {
      var Result = null;
      Result = rtl.as(this.Add(),$lt43);
      Result.FIDType = 0;
      Result.SetName(AName);
      Result.SetResultType(AResultType);
      Result.FOnGetVarValue = ACallback;
      return Result;
    };
    this.AddFloatVariable = function (AName, AValue) {
      var Result = null;
      Result = rtl.as(this.Add(),$lt43);
      Result.FIDType = 0;
      Result.SetName(AName);
      Result.SetResultType(2);
      Result.FValue.resValue = AValue;
      return Result;
    };
    this.AddFunction = function (AName, AResultType, AParamTypes, ACallBack) {
      var Result = null;
      Result = rtl.as(this.Add(),$lt43);
      Result.SetName(AName);
      Result.FIDType = 1;
      Result.SetArgumentTypes(AParamTypes);
      Result.SetResultType($mod.CharToResultType(AResultType));
      Result.FOnGetValue = ACallBack;
      return Result;
    };
    this.AddFunction$1 = function (AName, AResultType, AParamTypes, ANodeClass) {
      var Result = null;
      Result = rtl.as(this.Add(),$lt43);
      Result.SetName(AName);
      Result.FIDType = 2;
      Result.SetArgumentTypes(AParamTypes);
      Result.SetResultType($mod.CharToResultType(AResultType));
      Result.FNodeType = ANodeClass;
      return Result;
    };
  });
  rtl.createClass(this,"TFPExprIdentifierNode",$lt6,function () {
    $lt47 = this;
    this.$init = function () {
      $lt6.$init.call(this);
      this.FID = null;
      this.PResult = null;
      this.FResultType = 0;
    };
    this.$final = function () {
      this.FID = undefined;
      $lt6.$final.call(this);
    };
    this.CreateIdentifier = function (AID) {
      $lt60.Create.call(this);
      this.FID = AID;
      this.PResult = this.FID.FValue;
      this.FResultType = this.FID.GetResultType();
      return this;
    };
    this.NodeType = function () {
      var Result = 0;
      Result = this.FResultType;
      return Result;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      if (this.FID.EventBasedVariable()) {
        this.FID.FetchValue();
        this.PResult = this.FID.FValue;
      };
      Result.$assign(this.PResult);
      Result.ResultType = this.FResultType;
      return Result;
    };
  });
  rtl.createClass(this,"TFPExprVariable",$lt47,function () {
    $lt48 = this;
    this.Check = function () {
    };
    this.AsString = function () {
      var Result = "";
      Result = this.FID.FName;
      return Result;
    };
  });
  rtl.createClass(this,"TFPExprFunction",$lt47,function () {
    $lt49 = this;
    this.$init = function () {
      $lt47.$init.call(this);
      this.FArgumentNodes = [];
      this.FargumentParams = [];
    };
    this.$final = function () {
      this.FArgumentNodes = undefined;
      this.FargumentParams = undefined;
      $lt47.$final.call(this);
    };
    this.CalcParams = function () {
      var I = 0;
      for (var $l = 0, $end = rtl.length(this.FargumentParams) - 1; $l <= $end; $l++) {
        I = $l;
        this.FargumentParams[I].$assign(this.FArgumentNodes[I].GetNodeValue());
      };
    };
    this.ConvertArgument = function (aIndex, aNode, aType) {
      var Result = null;
      var N = null;
      N = $lt57.ConvertNode(aNode,aType);
      if (aNode === N) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrInvalidArgumentType"),$lp1(0,aIndex,18,$mod.ResultTypeName(aType),18,$mod.ResultTypeName(aNode.NodeType())));
      Result = N;
      return Result;
    };
    this.Check = function () {
      var I = 0;
      var rtp = 0;
      var rta = 0;
      if (rtl.length(this.FArgumentNodes) !== this.FID.ArgumentCount()) $impl.RaiseParserError$1(rtl.getResStr($mod,"ErrInvalidArgumentCount"),$lp1(18,this.FID.FName));
      for (var $l = 0, $end = rtl.length(this.FArgumentNodes) - 1; $l <= $end; $l++) {
        I = $l;
        rtp = $mod.CharToResultType(this.FID.FArgumentTypes.charAt((I + 1) - 1));
        rta = this.FArgumentNodes[I].NodeType();
        if (rtp !== rta) this.FArgumentNodes[I] = this.ConvertArgument(I + 1,this.FArgumentNodes[I],rtp);
      };
    };
    this.CreateFunction = function (AID, Args) {
      $lt47.CreateIdentifier.call(this,AID);
      this.FArgumentNodes = rtl.arrayRef(Args);
      this.FargumentParams = rtl.arraySetLength(this.FargumentParams,$lt5,rtl.length(Args));
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      for (var $l = 0, $end = rtl.length(this.FArgumentNodes) - 1; $l <= $end; $l++) {
        I = $l;
        $lp5({a: I, p: this.FArgumentNodes, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }});
      };
      $lt60.Destroy.call(this);
    };
    this.AsString = function () {
      var Result = "";
      var S = "";
      var I = 0;
      S = "";
      for (var $l = 0, $end = rtl.length(this.FArgumentNodes) - 1; $l <= $end; $l++) {
        I = $l;
        if (S !== "") S = S + ",";
        S = S + this.FArgumentNodes[I].AsString();
      };
      if (S !== "") S = "(" + S + ")";
      Result = this.FID.FName + S;
      return Result;
    };
  });
  rtl.createClass(this,"TAggregateExpr",$lt49,function () {
    $lt50 = this;
    this.$init = function () {
      $lt49.$init.call(this);
      this.FResult = $lt5.$new();
    };
    this.$final = function () {
      this.FResult = undefined;
      $lt49.$final.call(this);
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign(this.FResult);
      return Result;
    };
  });
  rtl.createClass(this,"TAggregateMin",$lt50,function () {
    $lt51 = this;
  });
  rtl.createClass(this,"TAggregateMax",$lt50,function () {
    $lt52 = this;
  });
  rtl.createClass(this,"TAggregateSum",$lt50,function () {
    $lt53 = this;
    this.ConvertArgument = function (aIndex, aNode, aType) {
      var Result = null;
      if (!(aNode.NodeType() in rtl.createSet(2,1,5))) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrInvalidArgumentType"),$lp1(0,aIndex,18,$mod.ResultTypeName(aType),18,$mod.ResultTypeName(aNode.NodeType())));
      Result = aNode;
      return Result;
    };
  });
  rtl.createClass(this,"TAggregateAvg",$lt53,function () {
    $lt54 = this;
    this.$init = function () {
      $lt53.$init.call(this);
      this.FCount = 0;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      Result.$assign($lt50.GetNodeValue.call(this));
      Result.ResultType = this.FResult.ResultType;
      if (this.FCount === 0) {
        var $tmp = this.FResult.ResultType;
        if ($tmp === 1) {
          Result.ResultType = 2;
          Result.resValue = 0.0;
        } else if ($tmp === 2) {
          Result.resValue = 0.0}
         else if ($tmp === 5) Result.resValue = 0.0;
      } else {
        var $tmp1 = this.FResult.ResultType;
        if ($tmp1 === 1) {
          Result.ResultType = 2;
          Result.resValue = rtl.trunc(this.FResult.resValue) / this.FCount;
        } else if ($tmp1 === 2) {
          Result.resValue = rtl.getNumber(this.FResult.resValue) / this.FCount}
         else if ($tmp1 === 5) Result.resValue = rtl.trunc(rtl.trunc(this.FResult.resValue * 10000) / this.FCount) / 10000;
      };
      return Result;
    };
  });
  rtl.createClass(this,"TAggregateCount",$lt50,function () {
    $lt55 = this;
  });
  rtl.createClass(this,"TFPFunctionEventHandler",$lt49,function () {
    $lt56 = this;
    this.$init = function () {
      $lt49.$init.call(this);
      this.FCallBack = null;
    };
    this.$final = function () {
      this.FCallBack = undefined;
      $lt49.$final.call(this);
    };
    this.CreateFunction = function (AID, Args) {
      $lt49.CreateFunction.apply(this,arguments);
      this.FCallBack = AID.FOnGetValue;
      return this;
    };
    this.GetNodeValue = function () {
      var Result = $lt5.$new();
      if (rtl.length(this.FargumentParams) > 0) this.CalcParams();
      Result.$assign(this.FCallBack(this.FargumentParams));
      Result.ResultType = this.NodeType();
      return Result;
    };
  });
  rtl.createClass(this,"TFPExpressionParser",$lt65,function () {
    $lt57 = this;
    this.$init = function () {
      $lt65.$init.call(this);
      this.FBuiltIns = {};
      this.FExpression = "";
      this.FScanner = null;
      this.FExprNode = null;
      this.FIdentifiers = null;
      this.FHashList = null;
      this.FDirty = false;
    };
    this.$final = function () {
      this.FBuiltIns = undefined;
      this.FScanner = undefined;
      this.FExprNode = undefined;
      this.FIdentifiers = undefined;
      this.FHashList = undefined;
      $lt65.$final.call(this);
    };
    this.CheckEOF = function () {
      if (this.TokenType() === 26) this.ParserError(rtl.getResStr($mod,"SErrUnexpectedEndOfExpression"));
    };
    this.GetAsBoolean = function () {
      var Result = false;
      var Res = $lt5.$new();
      this.EvaluateExpression(Res);
      this.CheckResultType(Res,0);
      Result = !(Res.resValue == false);
      return Result;
    };
    this.MatchNodes = function (Todo, Match) {
      var Result = null;
      var FromType = 0;
      var ToType = 0;
      Result = Todo;
      FromType = Todo.NodeType();
      ToType = Match.NodeType();
      if (FromType !== ToType) {
        var $tmp = FromType;
        if ($tmp === 1) {
          if (ToType in rtl.createSet(2,5,3)) Result = this.$class.ConvertNode(Todo,ToType)}
         else if ($tmp === 2) {
          if (ToType in rtl.createSet(5,3)) Result = this.$class.ConvertNode(Todo,ToType)}
         else if ($tmp === 5) if (ToType in rtl.createSet(2,3)) Result = this.$class.ConvertNode(Todo,ToType);
      };
      return Result;
    };
    this.CheckNodes = function (Left, Right) {
      Left.set(this.MatchNodes(Left.get(),Right.get()));
      Right.set(this.MatchNodes(Right.get(),Left.get()));
    };
    this.SetBuiltIns = function (AValue) {
      if (rtl.eqSet(this.FBuiltIns,AValue)) return;
      this.FBuiltIns = rtl.refSet(AValue);
      this.FDirty = true;
    };
    this.SetIdentifiers = function (AValue) {
      this.FIdentifiers.Assign(AValue);
    };
    this.ParserError = function (Msg) {
      throw $lt59.$create("Create$1",[Msg]);
    };
    this.SetExpression = function (AValue) {
      if (this.FExpression === AValue) return;
      this.FExpression = AValue;
      this.FScanner.SetSource(AValue);
      if (this.FExprNode != null) $lp5({p: this, get: function () {
          return this.p.FExprNode;
        }, set: function (v) {
          this.p.FExprNode = v;
        }});
      if (this.FExpression !== "") {
        this.GetToken();
        this.FExprNode = this.Level1();
        if (this.TokenType() !== 26) this.ParserError($lp(rtl.getResStr($mod,"SErrUnterminatedExpression"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())));
        this.FExprNode.Check();
      } else this.FExprNode = null;
    };
    this.CheckResultType = function (Res, AType) {
      if (Res.ResultType !== AType) $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrInvalidResultType"),$lp1(18,$mod.ResultTypeName(Res.ResultType)));
    };
    this.ConvertNode = function (Todo, ToType) {
      var Result = null;
      Result = Todo;
      var $tmp = Todo.NodeType();
      if ($tmp === 1) {
        var $tmp1 = ToType;
        if ($tmp1 === 2) {
          Result = $lt33.$create("Create$1",[Result])}
         else if ($tmp1 === 5) {
          Result = $lt34.$create("Create$1",[Result])}
         else if ($tmp1 === 3) Result = $lt35.$create("Create$1",[Result]);
      } else if ($tmp === 2) {
        var $tmp2 = ToType;
        if ($tmp2 === 5) {
          Result = $lt37.$create("Create$1",[Result])}
         else if ($tmp2 === 3) Result = $lt36.$create("Create$1",[Result]);
      } else if ($tmp === 5) {
        var $tmp3 = ToType;
        if ($tmp3 === 2) {
          Result = $lt39.$create("Create$1",[Result])}
         else if ($tmp3 === 3) Result = $lt38.$create("Create$1",[Result]);
      };
      return Result;
    };
    this.BuiltinsManager = function () {
      var Result = null;
      Result = $mod.BuiltinIdentifiers();
      return Result;
    };
    this.Level1 = function () {
      var Result = null;
      var tt = 0;
      var Right = null;
      if (this.TokenType() === 22) {
        this.GetToken();
        this.CheckEOF();
        Right = this.Level2();
        Result = $lt31.$create("Create$1",[Right]);
      } else Result = this.Level2();
      try {
        while (this.TokenType() in rtl.createSet(17,18,19)) {
          tt = this.TokenType();
          this.GetToken();
          this.CheckEOF();
          Right = this.Level2();
          var $tmp = tt;
          if ($tmp === 18) {
            Result = $lt10.$create("Create$1",[Result,Right])}
           else if ($tmp === 17) {
            Result = $lt9.$create("Create$1",[Result,Right])}
           else if ($tmp === 19) {
            Result = $lt11.$create("Create$1",[Result,Right])}
           else {
            this.ParserError(rtl.getResStr($mod,"SErrUnknownBooleanOp"));
          };
        };
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.Level2 = function () {
      var Result = null;
      var Right = null;
      var tt = 0;
      var C = null;
      Result = this.Level3();
      try {
        if (this.TokenType() in $mod.ttComparisons) {
          tt = this.TokenType();
          this.GetToken();
          this.CheckEOF();
          Right = this.Level3();
          this.CheckNodes({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},{get: function () {
              return Right;
            }, set: function (v) {
              Right = v;
            }});
          var $tmp = tt;
          if ($tmp === 2) {
            C = $lt16}
           else if ($tmp === 10) {
            C = $lt18}
           else if ($tmp === 3) {
            C = $lt17}
           else if ($tmp === 11) {
            C = $lt19}
           else if ($tmp === 4) {
            C = $lt13}
           else if ($tmp === 12) {
            C = $lt14}
           else {
            this.ParserError(rtl.getResStr($mod,"SErrUnknownComparison"));
          };
          Result = C.$create("Create$1",[Result,Right]);
        };
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.Level3 = function () {
      var Result = null;
      var tt = 0;
      var right = null;
      Result = this.Level4();
      try {
        while (this.TokenType() in rtl.createSet(0,1)) {
          tt = this.TokenType();
          this.GetToken();
          this.CheckEOF();
          right = this.Level4();
          this.CheckNodes({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},{get: function () {
              return right;
            }, set: function (v) {
              right = v;
            }});
          var $tmp = tt;
          if ($tmp === 0) {
            Result = $lt23.$create("Create$1",[Result,right])}
           else if ($tmp === 1) Result = $lt24.$create("Create$1",[Result,right]);
        };
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.Level4 = function () {
      var Result = null;
      var tt = 0;
      var right = null;
      Result = this.Level5();
      try {
        while (this.TokenType() in rtl.createSet(7,5,6)) {
          tt = this.TokenType();
          this.GetToken();
          right = this.Level5();
          this.CheckNodes({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},{get: function () {
              return right;
            }, set: function (v) {
              right = v;
            }});
          var $tmp = tt;
          if ($tmp === 7) {
            Result = $lt25.$create("Create$1",[Result,right])}
           else if ($tmp === 5) {
            Result = $lt26.$create("Create$1",[Result,right])}
           else if ($tmp === 6) Result = $lt27.$create("Create$1",[Result,right]);
        };
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.Level5 = function () {
      var Result = null;
      var B = false;
      B = false;
      if (this.TokenType() in rtl.createSet(0,1)) {
        B = this.TokenType() === 1;
        this.GetToken();
      };
      Result = this.Level6();
      if (B) Result = $lt40.$create("Create$1",[Result]);
      return Result;
    };
    this.Level6 = function () {
      var Result = null;
      var right = null;
      Result = this.Level7();
      try {
        while (this.TokenType() === 25) {
          this.GetToken();
          right = this.Level5();
          this.CheckNodes({get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }},{get: function () {
              return right;
            }, set: function (v) {
              right = v;
            }});
          Result = $lt28.$create("Create$1",[Result,right]);
        };
      } catch ($e) {
        Result = rtl.freeLoc(Result);
        throw $e;
      };
      return Result;
    };
    this.Level7 = function () {
      var Result = null;
      if (this.TokenType() === 8) {
        this.GetToken();
        Result = this.Level1();
        try {
          if (this.TokenType() !== 9) this.ParserError($lp(rtl.getResStr($mod,"SErrBracketExpected"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())));
          this.GetToken();
        } catch ($e) {
          Result = rtl.freeLoc(Result);
          throw $e;
        };
      } else Result = this.Primitive();
      return Result;
    };
    this.Primitive = function () {
      var Result = null;
      var I = 0;
      var C = 0;
      var X = 0.0;
      var ACount = 0;
      var IFF = false;
      var IFC = false;
      var ID = null;
      var Args = [];
      var AI = 0;
      Args = rtl.arraySetLength(Args,null,0);
      if (this.TokenType() === 13) {
        if ($lp17(this.CurrentToken(),{get: function () {
            return I;
          }, set: function (v) {
            I = v;
          }})) {
          Result = $lt41.$create("CreateInteger",[I])}
         else {
          $lp4(this.CurrentToken(),{get: function () {
              return X;
            }, set: function (v) {
              X = v;
            }},{get: function () {
              return C;
            }, set: function (v) {
              C = v;
            }});
          if (C === 0) {
            Result = $lt41.$create("CreateFloat",[X])}
           else this.ParserError($lp(rtl.getResStr($mod,"SErrInvalidFloat"),$lp1(18,this.CurrentToken())));
        };
      } else if (this.TokenType() === 14) {
        Result = $lt41.$create("CreateString",[this.CurrentToken()])}
       else if (this.TokenType() in rtl.createSet(20,21)) {
        Result = $lt41.$create("CreateBoolean",[this.TokenType() === 20])}
       else if (!(this.TokenType() in rtl.createSet(15,23,24))) {
        this.ParserError($lp(rtl.getResStr($mod,"SerrUnknownTokenAtPos"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())))}
       else {
        IFF = this.TokenType() === 23;
        IFC = this.TokenType() === 24;
        if (!(IFF || IFC)) {
          ID = this.IdentifierByName(this.CurrentToken());
          if (ID === null) this.ParserError($lp(rtl.getResStr($mod,"SErrUnknownIdentifier"),$lp1(18,this.CurrentToken())));
        };
        if (IFF) {
          ACount = 3}
         else if (IFC) {
          ACount = -4}
         else if (ID.FIDType in rtl.createSet(1,2)) {
          ACount = ID.ArgumentCount()}
         else ACount = 0;
        if (ACount !== 0) {
          this.GetToken();
          if (this.TokenType() !== 8) this.ParserError($lp(rtl.getResStr($mod,"SErrLeftBracketExpected"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())));
          Args = rtl.arraySetLength(Args,null,Math.abs(ACount));
          AI = 0;
          try {
            do {
              this.GetToken();
              if ((ACount < 0) && (AI === rtl.length(Args))) {
                Args = rtl.arraySetLength(Args,null,AI + 1);
                Args[AI] = null;
              };
              Args[AI] = this.Level1();
              AI += 1;
              if (this.TokenType() !== 16) if (AI < Math.abs(ACount)) this.ParserError($lp(rtl.getResStr($mod,"SErrCommaExpected"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())));
            } while (!((AI === ACount) || ((ACount < 0) && (this.TokenType() === 9))));
            if (this.TokenType() !== 9) this.ParserError($lp(rtl.getResStr($mod,"SErrBracketExpected"),$lp1(0,this.FScanner.FPos,18,this.CurrentToken())));
          } catch ($e) {
            if ($lt61.isPrototypeOf($e)) {
              var E = $e;
              AI -= 1;
              while (AI >= 0) {
                $lp5({a: AI, p: Args, get: function () {
                    return this.p[this.a];
                  }, set: function (v) {
                    this.p[this.a] = v;
                  }});
                AI -= 1;
              };
              throw $e;
            } else throw $e
          };
        };
        if (IFF) {
          Result = $lt20.$create("Create$2",[Args[0],Args[1],Args[2]])}
         else if (IFC) {
          Result = $lt21.$create("Create$1",[rtl.arrayRef(Args)])}
         else {
          var $tmp = ID.FIDType;
          if ($tmp === 0) {
            Result = $lt48.$create("CreateIdentifier",[ID])}
           else if ($tmp === 1) {
            Result = $lt56.$create("CreateFunction",[ID,Args])}
           else if ($tmp === 2) Result = ID.FNodeType.$create("CreateFunction",[ID,Args]);
        };
      };
      this.GetToken();
      return Result;
    };
    this.GetToken = function () {
      var Result = 0;
      Result = this.FScanner.GetToken();
      return Result;
    };
    this.TokenType = function () {
      var Result = 0;
      Result = this.FScanner.FTokenType;
      return Result;
    };
    this.CurrentToken = function () {
      var Result = "";
      Result = this.FScanner.FToken;
      return Result;
    };
    this.CreateHashList = function () {
      var ID = null;
      var BID = null;
      var I = 0;
      var M = null;
      this.FHashList.Clear();
      M = this.$class.BuiltinsManager();
      if (rtl.neSet(this.FBuiltIns,{}) && (M != null)) for (var $l = 0, $end = M.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        BID = M.GetI(I);
        if (BID.FCategory in this.FBuiltIns) this.FHashList.Add($lp2(BID.FName),BID);
      };
      for (var $l1 = 0, $end1 = this.FIdentifiers.GetCount() - 1; $l1 <= $end1; $l1++) {
        I = $l1;
        ID = this.FIdentifiers.GetI(I);
        this.FHashList.Add($lp2(ID.FName),ID);
      };
      this.FDirty = false;
    };
    this.Create$1 = function (AOwner) {
      $lt65.Create$1.call(this,AOwner);
      this.FIdentifiers = $lt46.$create("Create$1",[$lt43]);
      this.FIdentifiers.FParser = this;
      this.FScanner = $lt2.$create("Create$1");
      this.FHashList = $lt66.$create("Create$2",[false]);
      return this;
    };
    this.Destroy = function () {
      $lp5({p: this, get: function () {
          return this.p.FHashList;
        }, set: function (v) {
          this.p.FHashList = v;
        }});
      $lp5({p: this, get: function () {
          return this.p.FExprNode;
        }, set: function (v) {
          this.p.FExprNode = v;
        }});
      $lp5({p: this, get: function () {
          return this.p.FIdentifiers;
        }, set: function (v) {
          this.p.FIdentifiers = v;
        }});
      $lp5({p: this, get: function () {
          return this.p.FScanner;
        }, set: function (v) {
          this.p.FScanner = v;
        }});
      $lt65.Destroy.call(this);
    };
    this.IdentifierByName = function (AName) {
      var Result = null;
      if (this.FDirty) this.CreateHashList();
      Result = this.FHashList.GetData($lp2(AName));
      return Result;
    };
    this.EvaluateExpression = function (Result) {
      Result.$assign(this.Evaluate$1());
    };
    this.Evaluate$1 = function () {
      var Result = $lt5.$new();
      if (this.FExpression === "") this.ParserError(rtl.getResStr($mod,"SErrInExpressionEmpty"));
      if (!(this.FExprNode != null)) this.ParserError(rtl.getResStr($mod,"SErrInExpression"));
      Result.$assign(this.FExprNode.GetNodeValue());
      return Result;
    };
    var $r = this.$rtti;
    $r.addProperty("Expression",2,rtl.string,"FExpression","SetExpression");
    $r.addProperty("Identifiers",2,$mod.$rtti["TFPExprIdentifierDefs"],"FIdentifiers","SetIdentifiers");
    $r.addProperty("BuiltIns",2,$mod.$rtti["TBuiltInCategories"],"FBuiltIns","SetBuiltIns");
  });
  rtl.createClass(this,"TExprBuiltInManager",$lt65,function () {
    $lt58 = this;
    this.$init = function () {
      $lt65.$init.call(this);
      this.FDefs = null;
    };
    this.$final = function () {
      this.FDefs = undefined;
      $lt65.$final.call(this);
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FDefs.GetCount();
      return Result;
    };
    this.GetI = function (AIndex) {
      var Result = null;
      Result = this.FDefs.GetI(AIndex);
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt65.Create$1.call(this,AOwner);
      this.FDefs = $lt46.$create("Create$1",[$lt45]);
      return this;
    };
    this.Destroy = function () {
      $lp5({p: this, get: function () {
          return this.p.FDefs;
        }, set: function (v) {
          this.p.FDefs = v;
        }});
      $lt65.Destroy.call(this);
    };
    this.AddFloatVariable = function (ACategory, AName, AValue) {
      var Result = null;
      Result = this.FDefs.AddFloatVariable(AName,AValue);
      Result.FCategory = ACategory;
      return Result;
    };
    this.AddFunction = function (ACategory, AName, AResultType, AParamTypes, ACallBack) {
      var Result = null;
      Result = this.FDefs.AddFunction(AName,AResultType,AParamTypes,ACallBack);
      Result.FCategory = ACategory;
      return Result;
    };
    this.AddFunction$1 = function (ACategory, AName, AResultType, AParamTypes, ANodeClass) {
      var Result = null;
      Result = this.FDefs.AddFunction$1(AName,AResultType,AParamTypes,ANodeClass);
      Result.FCategory = ACategory;
      return Result;
    };
  });
  rtl.createClass(this,"EExprParser",$lt61,function () {
    $lt59 = this;
  });
  this.AllBuiltIns = rtl.createSet(0,1,2,3,4,5,6,7,8);
  this.ResultTypeName = function (AResult) {
    var Result = "";
    Result = $lp15($mod.$rtti["TResultType"],AResult);
    return Result;
  };
  this.CharToResultType = function (C) {
    var Result = 0;
    var $tmp = $lp3(C);
    if ($tmp === "S") {
      Result = 4}
     else if ($tmp === "D") {
      Result = 3}
     else if ($tmp === "B") {
      Result = 0}
     else if ($tmp === "I") {
      Result = 1}
     else if ($tmp === "F") {
      Result = 2}
     else if ($tmp === "C") {
      Result = 5}
     else {
      $impl.RaiseParserError$1(rtl.getResStr($mod,"SErrInvalidResultCharacter"),$lp1(9,C));
    };
    return Result;
  };
  this.BuiltinIdentifiers = function () {
    var Result = null;
    if ($impl.BuiltIns === null) $impl.BuiltIns = $lt58.$create("Create$1",[null]);
    Result = $impl.BuiltIns;
    return Result;
  };
  this.RegisterStdBuiltins = function (AManager, Categories) {
    if (2 in Categories) {
      AManager.AddFloatVariable(2,"pi",Math.PI);
      AManager.AddFunction(2,"cos","F","F",$impl.BuiltInCos);
      AManager.AddFunction(2,"sin","F","F",$impl.BuiltInSin);
      AManager.AddFunction(2,"abs","F","F",$impl.BuiltInAbs);
      AManager.AddFunction(2,"sqr","F","F",$impl.BuiltInSqr);
      AManager.AddFunction(2,"sqrt","F","F",$impl.BuiltInSqrt);
      AManager.AddFunction(2,"exp","F","F",$impl.BuiltInExp);
      AManager.AddFunction(2,"ln","F","F",$impl.BuiltInLn);
      AManager.AddFunction(2,"log","F","F",$impl.BuiltInLog);
      AManager.AddFunction(2,"frac","F","F",$impl.BuiltInFrac);
      AManager.AddFunction(2,"int","F","F",$impl.BuiltInInt);
      AManager.AddFunction(2,"round","I","F",$impl.BuiltInRound);
      AManager.AddFunction(2,"trunc","I","F",$impl.BuiltInTrunc);
    };
    if (0 in Categories) {
      AManager.AddFunction(0,"length","I","S",$impl.BuiltInLength);
      AManager.AddFunction(0,"copy","S","SII",$impl.BuiltInCopy);
      AManager.AddFunction(0,"delete","S","SII",$impl.BuiltInDelete);
      AManager.AddFunction(0,"pos","I","SS",$impl.BuiltInPos);
      AManager.AddFunction(0,"lowercase","S","S",$impl.BuiltInLowercase);
      AManager.AddFunction(0,"uppercase","S","S",$impl.BuiltInUppercase);
      AManager.AddFunction(0,"stringreplace","S","SSSBB",$impl.BuiltInStringReplace);
      AManager.AddFunction(0,"comparetext","I","SS",$impl.BuiltInCompareText);
    };
    if (1 in Categories) {
      AManager.AddFunction(1,"date","D","",$impl.BuiltInDate);
      AManager.AddFunction(1,"time","D","",$impl.BuiltInTime);
      AManager.AddFunction(1,"now","D","",$impl.BuiltInNow);
      AManager.AddFunction(1,"dayofweek","I","D",$impl.BuiltInDayofWeek);
      AManager.AddFunction(1,"extractyear","I","D",$impl.BuiltInExtractYear);
      AManager.AddFunction(1,"extractmonth","I","D",$impl.BuiltInExtractMonth);
      AManager.AddFunction(1,"extractday","I","D",$impl.BuiltInExtractDay);
      AManager.AddFunction(1,"extracthour","I","D",$impl.BuiltInExtractHour);
      AManager.AddFunction(1,"extractmin","I","D",$impl.BuiltInExtractMin);
      AManager.AddFunction(1,"extractsec","I","D",$impl.BuiltInExtractSec);
      AManager.AddFunction(1,"extractmsec","I","D",$impl.BuiltInExtractMSec);
      AManager.AddFunction(1,"encodedate","D","III",$impl.BuiltInEncodedate);
      AManager.AddFunction(1,"encodetime","D","IIII",$impl.BuiltInEncodeTime);
      AManager.AddFunction(1,"encodedatetime","D","IIIIIII",$impl.BuiltInEncodeDateTime);
      AManager.AddFunction(1,"shortdayname","S","I",$impl.BuiltInShortDayName);
      AManager.AddFunction(1,"shortmonthname","S","I",$impl.BuiltInShortMonthName);
      AManager.AddFunction(1,"longdayname","S","I",$impl.BuiltInLongDayName);
      AManager.AddFunction(1,"longmonthname","S","I",$impl.BuiltInLongMonthName);
    };
    if (3 in Categories) {
      AManager.AddFunction(3,"shl","I","II",$impl.BuiltInShl);
      AManager.AddFunction(3,"shr","I","II",$impl.BuiltInShr);
      AManager.AddFunction(3,"IFS","S","BSS",$impl.BuiltinIFS);
      AManager.AddFunction(3,"IFF","F","BFF",$impl.BuiltinIFF);
      AManager.AddFunction(3,"IFD","D","BDD",$impl.BuiltinIFD);
      AManager.AddFunction(3,"IFI","I","BII",$impl.BuiltinIFI);
    };
    if (4 in Categories) {
      AManager.AddFunction(4,"inttostr","S","I",$impl.BuiltInIntToStr);
      AManager.AddFunction(4,"strtoint","I","S",$impl.BuiltInStrToInt);
      AManager.AddFunction(4,"strtointdef","I","SI",$impl.BuiltInStrToIntDef);
      AManager.AddFunction(4,"floattostr","S","F",$impl.BuiltInFloatToStr);
      AManager.AddFunction(4,"strtofloat","F","S",$impl.BuiltInStrToFloat);
      AManager.AddFunction(4,"strtofloatdef","F","SF",$impl.BuiltInStrToFloatDef);
      AManager.AddFunction(4,"booltostr","S","B",$impl.BuiltInBoolToStr);
      AManager.AddFunction(4,"strtobool","B","S",$impl.BuiltInStrToBool);
      AManager.AddFunction(4,"strtobooldef","B","SB",$impl.BuiltInStrToBoolDef);
      AManager.AddFunction(4,"datetostr","S","D",$impl.BuiltInDateToStr);
      AManager.AddFunction(4,"timetostr","S","D",$impl.BuiltInTimeToStr);
      AManager.AddFunction(4,"strtodate","D","S",$impl.BuiltInStrToDate);
      AManager.AddFunction(4,"strtodatedef","D","SD",$impl.BuiltInStrToDateDef);
      AManager.AddFunction(4,"strtotime","D","S",$impl.BuiltInStrToTime);
      AManager.AddFunction(4,"strtotimedef","D","SD",$impl.BuiltInStrToTimeDef);
      AManager.AddFunction(4,"strtodatetime","D","S",$impl.BuiltInStrToDateTime);
      AManager.AddFunction(4,"strtodatetimedef","D","SD",$impl.BuiltInStrToDateTimeDef);
      AManager.AddFunction(4,"formatfloat","S","SF",$impl.BuiltInFormatFloat);
      AManager.AddFunction(4,"formatdatetime","S","SD",$impl.BuiltInFormatDateTime);
    };
    if (8 in Categories) {
      AManager.AddFunction$1(8,"count","I","",$lt55);
      AManager.AddFunction$1(8,"sum","F","F",$lt53);
      AManager.AddFunction$1(8,"avg","F","F",$lt54);
      AManager.AddFunction$1(8,"min","F","F",$lt51);
      AManager.AddFunction$1(8,"max","F","F",$lt52);
    };
  };
  this.ArgToFloat = function (Arg) {
    var Result = 0.0;
    if (Arg.ResultType === 1) {
      Result = rtl.trunc(Arg.resValue)}
     else if (Arg.ResultType === 5) {
      Result = rtl.trunc(Arg.resValue * 10000) / 10000}
     else Result = rtl.getNumber(Arg.resValue);
    return Result;
  };
  $mod.$implcode = function () {
    $lm3 = pas.TypInfo;
    $lp15 = $lm3.GetEnumName;
    $impl.cNull = "\x00";
    $impl.cSingleQuote = "'";
    $impl.cHexIdentifier = "$";
    $impl.cOctalIdentifier = "&";
    $impl.cBinaryIdentifier = "%";
    $impl.Digits = rtl.createSet(null,48,57,46);
    $impl.HexDigits = rtl.createSet(null,48,57,null,65,70,null,97,102);
    $impl.OctalDigits = rtl.createSet(null,48,55);
    $impl.BinaryDigits = rtl.createSet(48,49);
    $impl.WhiteSpace = rtl.createSet(32,13,10,9);
    $impl.Operators = rtl.createSet(43,45,60,62,61,47,42,94);
    $impl.Delimiters = rtl.unionSet($impl.Operators,rtl.createSet(44,40,41));
    $impl.Symbols = rtl.unionSet(rtl.createSet(37),$impl.Delimiters);
    $impl.WordDelimiters = rtl.unionSet($impl.WhiteSpace,$impl.Symbols);
    $impl.FileFormatSettings = $lt67.$new();
    $impl.RaiseParserError = function (Msg) {
      throw $lt59.$create("Create$1",[Msg]);
    };
    $impl.RaiseParserError$1 = function (Fmt, Args) {
      throw $lt59.$create("CreateFmt",[Fmt,Args]);
    };
    $impl.BuiltIns = null;
    $impl.power = function (base, exponent) {
      var Result = 0.0;
      var ex = 0;
      if (exponent === 0.0) {
        Result = 1.0}
       else if ((base === 0.0) && (exponent > 0.0)) {
        Result = 0.0}
       else if ((base < 0.0) && ($lp18(exponent) === 0.0)) {
        ex = Math.round(exponent);
        Result = Math.exp(exponent * Math.log(-base));
        if ($lp19(ex)) Result = -Result;
      } else Result = Math.exp(exponent * Math.log(base));
      return Result;
    };
    $impl.BuiltInCos = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.cos($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInSin = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.sin($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInAbs = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.abs($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInSqr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = $lp20($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInSqrt = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.sqrt($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInExp = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.exp($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInLn = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.log($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.L10 = 0.0;
    $impl.BuiltInLog = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = Math.log($mod.ArgToFloat($lt5.$clone(Args[0]))) / $impl.L10;
      return Result;
    };
    $impl.BuiltInRound = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = Math.round($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInTrunc = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp21($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInInt = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp22($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInFrac = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = $lp18($mod.ArgToFloat($lt5.$clone(Args[0])));
      return Result;
    };
    $impl.BuiltInLength = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = ("" + Args[0].resValue).length;
      return Result;
    };
    $impl.BuiltInCopy = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp23("" + Args[0].resValue,rtl.trunc(Args[1].resValue),rtl.trunc(Args[2].resValue));
      return Result;
    };
    $impl.BuiltInDelete = function (Args) {
      var Result = $lt5.$new();
      var S = "";
      Result.ResultType = 4;
      S = "" + Args[0].resValue;
      $lp24({get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},rtl.trunc(Args[1].resValue),rtl.trunc(Args[2].resValue));
      Result.resValue = S;
      return Result;
    };
    $impl.BuiltInPos = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp25("" + Args[0].resValue,"" + Args[1].resValue);
      return Result;
    };
    $impl.BuiltInUppercase = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp26("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInLowercase = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp2("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStringReplace = function (Args) {
      var Result = $lt5.$new();
      var F = {};
      Result.ResultType = 4;
      F = {};
      if (!(Args[3].resValue == false)) F = rtl.includeSet(F,0);
      if (!(Args[4].resValue == false)) F = rtl.includeSet(F,1);
      Result.resValue = $lp27("" + Args[0].resValue,"" + Args[1].resValue,"" + Args[2].resValue,rtl.refSet(F));
      return Result;
    };
    $impl.BuiltInCompareText = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp16("" + Args[0].resValue,"" + Args[1].resValue);
      return Result;
    };
    $impl.BuiltInDate = function (Args) {
      var Result = $lt5.$new();
      if (rtl.length(Args) === 0) ;
      Result.ResultType = 3;
      Result.resValue = $lp28();
      return Result;
    };
    $impl.BuiltInTime = function (Args) {
      var Result = $lt5.$new();
      if (rtl.length(Args) === 0) ;
      Result.ResultType = 3;
      Result.resValue = $lp29();
      return Result;
    };
    $impl.BuiltInNow = function (Args) {
      var Result = $lt5.$new();
      if (rtl.length(Args) === 0) ;
      Result.ResultType = 3;
      Result.resValue = $lp30();
      return Result;
    };
    $impl.BuiltInDayofWeek = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp31(rtl.getNumber(Args[0].resValue));
      return Result;
    };
    $impl.BuiltInExtractYear = function (Args) {
      var Result = $lt5.$new();
      var Y = 0;
      var M = 0;
      var D = 0;
      Result.ResultType = 1;
      $lp32(rtl.getNumber(Args[0].resValue),{get: function () {
          return Y;
        }, set: function (v) {
          Y = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return D;
        }, set: function (v) {
          D = v;
        }});
      Result.resValue = Y;
      return Result;
    };
    $impl.BuiltInExtractMonth = function (Args) {
      var Result = $lt5.$new();
      var Y = 0;
      var M = 0;
      var D = 0;
      Result.ResultType = 1;
      $lp32(rtl.getNumber(Args[0].resValue),{get: function () {
          return Y;
        }, set: function (v) {
          Y = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return D;
        }, set: function (v) {
          D = v;
        }});
      Result.resValue = M;
      return Result;
    };
    $impl.BuiltInExtractDay = function (Args) {
      var Result = $lt5.$new();
      var Y = 0;
      var M = 0;
      var D = 0;
      Result.ResultType = 1;
      $lp32(rtl.getNumber(Args[0].resValue),{get: function () {
          return Y;
        }, set: function (v) {
          Y = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return D;
        }, set: function (v) {
          D = v;
        }});
      Result.resValue = D;
      return Result;
    };
    $impl.BuiltInExtractHour = function (Args) {
      var Result = $lt5.$new();
      var H = 0;
      var M = 0;
      var S = 0;
      var MS = 0;
      Result.ResultType = 1;
      $lp33(rtl.getNumber(Args[0].resValue),{get: function () {
          return H;
        }, set: function (v) {
          H = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},{get: function () {
          return MS;
        }, set: function (v) {
          MS = v;
        }});
      Result.resValue = H;
      return Result;
    };
    $impl.BuiltInExtractMin = function (Args) {
      var Result = $lt5.$new();
      var H = 0;
      var M = 0;
      var S = 0;
      var MS = 0;
      Result.ResultType = 1;
      $lp33(rtl.getNumber(Args[0].resValue),{get: function () {
          return H;
        }, set: function (v) {
          H = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},{get: function () {
          return MS;
        }, set: function (v) {
          MS = v;
        }});
      Result.resValue = M;
      return Result;
    };
    $impl.BuiltInExtractSec = function (Args) {
      var Result = $lt5.$new();
      var H = 0;
      var M = 0;
      var S = 0;
      var MS = 0;
      Result.ResultType = 1;
      $lp33(rtl.getNumber(Args[0].resValue),{get: function () {
          return H;
        }, set: function (v) {
          H = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},{get: function () {
          return MS;
        }, set: function (v) {
          MS = v;
        }});
      Result.resValue = S;
      return Result;
    };
    $impl.BuiltInExtractMSec = function (Args) {
      var Result = $lt5.$new();
      var H = 0;
      var M = 0;
      var S = 0;
      var MS = 0;
      Result.ResultType = 1;
      $lp33(rtl.getNumber(Args[0].resValue),{get: function () {
          return H;
        }, set: function (v) {
          H = v;
        }},{get: function () {
          return M;
        }, set: function (v) {
          M = v;
        }},{get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},{get: function () {
          return MS;
        }, set: function (v) {
          MS = v;
        }});
      Result.resValue = MS;
      return Result;
    };
    $impl.BuiltInEncodedate = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp34(rtl.trunc(Args[0].resValue),rtl.trunc(Args[1].resValue),rtl.trunc(Args[2].resValue));
      return Result;
    };
    $impl.BuiltInEncodeTime = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp35(rtl.trunc(Args[0].resValue),rtl.trunc(Args[1].resValue),rtl.trunc(Args[2].resValue),rtl.trunc(Args[3].resValue));
      return Result;
    };
    $impl.BuiltInEncodeDateTime = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp34(rtl.trunc(Args[0].resValue),rtl.trunc(Args[1].resValue),rtl.trunc(Args[2].resValue)) + $lp35(rtl.trunc(Args[3].resValue),rtl.trunc(Args[4].resValue),rtl.trunc(Args[5].resValue),rtl.trunc(Args[6].resValue));
      return Result;
    };
    $impl.BuiltInShortDayName = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lm1.FormatSettings.ShortDayNames[rtl.trunc(Args[0].resValue)];
      return Result;
    };
    $impl.BuiltInShortMonthName = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lm1.FormatSettings.ShortMonthNames[rtl.trunc(Args[0].resValue) - 1];
      return Result;
    };
    $impl.BuiltInLongDayName = function (Args) {
      var Result = $lt5.$new();
      Result.resValue = $lm1.FormatSettings.LongDayNames[rtl.trunc(Args[0].resValue)];
      return Result;
    };
    $impl.BuiltInLongMonthName = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lm1.FormatSettings.LongMonthNames[rtl.trunc(Args[0].resValue) - 1];
      return Result;
    };
    $impl.BuiltInFormatDateTime = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp8("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInIntToStr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp7(rtl.trunc(Args[0].resValue));
      return Result;
    };
    $impl.BuiltInStrToInt = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp11("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToIntDef = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = $lp36("" + Args[0].resValue,rtl.trunc(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInFloatToStr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp9(rtl.getNumber(Args[0].resValue));
      return Result;
    };
    $impl.BuiltInStrToFloat = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = $lp12("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToFloatDef = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      Result.resValue = $lp37("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInDateToStr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp38(rtl.getNumber(Args[0].resValue));
      return Result;
    };
    $impl.BuiltInTimeToStr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp39(rtl.getNumber(Args[0].resValue));
      return Result;
    };
    $impl.BuiltInStrToDate = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp40("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToDateDef = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp41("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInStrToTime = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp42("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToTimeDef = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp43("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInStrToDateTime = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp14("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToDateTimeDef = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      Result.resValue = $lp44("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInFormatFloat = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp45("" + Args[0].resValue,rtl.getNumber(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInBoolToStr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      Result.resValue = $lp46(!(Args[0].resValue == false),false);
      return Result;
    };
    $impl.BuiltInStrToBool = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 0;
      Result.resValue = $lp47("" + Args[0].resValue);
      return Result;
    };
    $impl.BuiltInStrToBoolDef = function (Args) {
      var Result = $lt5.$new();
      Result.resValue = $lp48("" + Args[0].resValue,!(Args[1].resValue == false));
      Result.ResultType = 0;
      return Result;
    };
    $impl.BuiltInShl = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = rtl.shl(rtl.trunc(Args[0].resValue),rtl.trunc(Args[1].resValue));
      return Result;
    };
    $impl.BuiltInShr = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      Result.resValue = rtl.shr(rtl.trunc(Args[0].resValue),rtl.trunc(Args[1].resValue));
      return Result;
    };
    $impl.BuiltinIFS = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 4;
      if (!(Args[0].resValue == false)) {
        Result.resValue = Args[1].resValue}
       else Result.resValue = Args[2].resValue;
      return Result;
    };
    $impl.BuiltinIFI = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 1;
      if (!(Args[0].resValue == false)) {
        Result.resValue = Args[1].resValue}
       else Result.resValue = Args[2].resValue;
      return Result;
    };
    $impl.BuiltinIFF = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 2;
      if (!(Args[0].resValue == false)) {
        Result.resValue = Args[1].resValue}
       else Result.resValue = Args[2].resValue;
      return Result;
    };
    $impl.BuiltinIFD = function (Args) {
      var Result = $lt5.$new();
      Result.ResultType = 3;
      if (!(Args[0].resValue == false)) {
        Result.resValue = Args[1].resValue}
       else Result.resValue = Args[2].resValue;
      return Result;
    };
    $impl.InitFileFormatSettings = function () {
      $impl.FileFormatSettings.$assign($lm1.FormatSettings);
      $impl.FileFormatSettings.DecimalSeparator = ".";
      $impl.FileFormatSettings.DateSeparator = "-";
      $impl.FileFormatSettings.TimeSeparator = ":";
      $impl.FileFormatSettings.ShortDateFormat = "yyyy-mm-dd";
      $impl.FileFormatSettings.LongTimeFormat = "hh:nn:ss";
    };
    $mod.$resourcestrings = {SBadQuotes: {org: "Unterminated string"}, SUnknownDelimiter: {org: 'Unknown delimiter character: "%s"'}, SErrUnknownCharacter: {org: 'Unknown character at pos %d: "%s"'}, SErrUnexpectedEndOfExpression: {org: "Unexpected end of expression"}, SErrUnknownComparison: {org: "Internal error: Unknown comparison"}, SErrUnknownBooleanOp: {org: "Internal error: Unknown boolean operation"}, SErrBracketExpected: {org: "Expected ) bracket at position %d, but got %s"}, SerrUnknownTokenAtPos: {org: "Unknown token at pos %d : %s"}, SErrLeftBracketExpected: {org: "Expected ( bracket at position %d, but got %s"}, SErrInvalidFloat: {org: "%s is not a valid floating-point value"}, SErrUnknownIdentifier: {org: "Unknown identifier: %s"}, SErrInExpression: {org: "Cannot evaluate: error in expression"}, SErrInExpressionEmpty: {org: "Cannot evaluate: empty expression"}, SErrCommaExpected: {org: "Expected comma (,) at position %d, but got %s"}, SErrInvalidNumberChar: {org: "Unexpected character in number : %s"}, SErrInvalidNumber: {org: "Invalid numerical value : %s"}, SErrUnterminatedIdentifier: {org: "Unterminated quoted identifier: %s"}, SErrNoOperand: {org: "No operand for unary operation %s"}, SErrNoleftOperand: {org: "No left operand for binary operation %s"}, SErrNoRightOperand: {org: "No right operand for binary operation %s"}, SErrNoNegation: {org: "Cannot negate expression of type %s : %s"}, SErrNoNOTOperation: {org: 'Cannot perform "not" on expression of type %s: %s'}, SErrTypesDoNotMatch: {org: 'Type mismatch: %s<>%s for expressions "%s" and "%s".'}, SErrNoNodeToCheck: {org: "Internal error: No node to check !"}, SInvalidNodeType: {org: "Node type (%s) not in allowed types (%s) for expression: %s"}, SErrUnterminatedExpression: {org: "Badly terminated expression. Found token at position %d : %s"}, SErrDuplicateIdentifier: {org: 'An identifier with name "%s" already exists.'}, SErrInvalidResultCharacter: {org: '"%s" is not a valid return type indicator'}, ErrInvalidArgumentCount: {org: "Invalid argument count for function %s"}, SErrInvalidArgumentType: {org: "Invalid type for argument %d: Expected %s, got %s"}, SErrInvalidResultType: {org: "Invalid result type: %s"}, SErrIFNeedsBoolean: {org: "First argument to IF must be of type boolean: %s"}, SErrCaseNeeds3: {org: "Case statement needs to have at least 4 arguments"}, SErrCaseEvenCount: {org: "Case statement needs to have an even number of arguments"}, SErrCaseLabelNotAConst: {org: 'Case label %d "%s" is not a constant expression'}, SErrCaseLabelType: {org: 'Case label %d "%s" needs type %s, but has type %s'}, SErrCaseValueType: {org: 'Case value %d "%s" needs type %s, but has type %s'}};
  };
  $mod.$init = function () {
    $impl.L10 = Math.log(10);
    $mod.RegisterStdBuiltins($mod.BuiltinIdentifiers(),$mod.AllBuiltIns);
    $impl.InitFileFormatSettings();
  };
},["TypInfo"]);
rtl.module("JSONDataset",["System","Types","JS","DB","Classes","SysUtils","TypInfo","fpexprpars"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lt6 = null;
  var $lt7 = null;
  var $lt8 = null;
  var $lt9 = null;
  var $lt10 = null;
  var $lt11 = null;
  var $lt12 = null;
  var $lt13 = null;
  var $lt14 = null;
  var $lt15 = null;
  var $lt16 = null;
  var $lt17 = null;
  var $lt18 = null;
  var $lt19 = null;
  var $lt20 = null;
  var $lm = pas.System;
  var $lt21 = $lm.TObject;
  var $lm1 = pas.DB;
  var $lt22 = $lm1.TDateField;
  var $lt23 = $lm1.TTimeField;
  var $lt24 = $lm1.TDateTimeField;
  var $lp = $lm.Copy;
  var $lm2 = pas.SysUtils;
  var $lp1 = $lm2.CompareText;
  var $lp2 = $lm2.CompareStr;
  var $lm3 = pas.JS;
  var $lp3 = $lm3.isDate;
  var $lp4 = $lm2.JSDateToDateTime;
  var $lm4 = pas.Classes;
  var $lt25 = $lm4.TFPList;
  var $lt26 = $lm1.EDatabaseError;
  var $lp5 = $lm.VarRecs;
  var $lm5 = pas.TypInfo;
  var $lp6 = $lm5.GetEnumName;
  var $lp7 = $lm3.isUndefined;
  var $lp8 = $lm2.FreeAndNil;
  var $lt27 = $lm1.TIndexDef;
  var $lt28 = $lm1.TIndexDefs;
  var $lt29 = $lm1.TDataSet;
  var $lm6 = pas.fpexprpars;
  var $lt30 = $lm6.TFPExpressionResult;
  var $lp9 = $lm1.DatabaseErrorFmt$1;
  var $lt31 = $lm6.TFPExpressionParser;
  var $lt32 = $lm1.TDataRecord;
  var $lt33 = $lm1.TBookmark;
  var $lp10 = $lm1.DatabaseErrorFmt;
  var $lm7 = null;
  var $lp11 = null;
  var $lp12 = null;
  var $lp13 = $lm2.FormatDateTime;
  var $lp14 = $lm3.JSDelete;
  $lt = this.TJSONRowType = {"0": "rtJSONObject", rtJSONObject: 0, "1": "rtJSONArray", rtJSONArray: 1};
  rtl.createClass(this,"TJSONFieldMapper",$lt21,function () {
    $lt1 = this;
    this.CopyRow = function (aRow) {
      var Result = undefined;
      Result = JSON.parse(JSON.stringify(aRow));
      return Result;
    };
    this.GetJSONDataForField$1 = function (F, Row) {
      var Result = undefined;
      Result = this.GetJSONDataForField(F.FFieldName,F.GetIndex(),Row);
      return Result;
    };
    this.SetJSONDataForField$1 = function (F, Row, Data) {
      this.SetJSONDataForField(F.FFieldName,F.GetIndex(),Row,Data);
    };
  });
  rtl.createClass(this,"TJSONDateField",$lt22,function () {
    $lt2 = this;
    this.$init = function () {
      $lt22.$init.call(this);
      this.FDateFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateFormat",0,rtl.string,"FDateFormat","FDateFormat");
  });
  rtl.createClass(this,"TJSONTimeField",$lt23,function () {
    $lt3 = this;
    this.$init = function () {
      $lt23.$init.call(this);
      this.FTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("TimeFormat",0,rtl.string,"FTimeFormat","FTimeFormat");
  });
  rtl.createClass(this,"TJSONDateTimeField",$lt24,function () {
    $lt4 = this;
    this.$init = function () {
      $lt24.$init.call(this);
      this.FDateTimeFormat = "";
    };
    var $r = this.$rtti;
    $r.addProperty("DateTimeFormat",0,rtl.string,"FDateTimeFormat","FDateTimeFormat");
  });
  rtl.createClass(this,"TFieldComparer",$lt21,function () {
    $lt5 = this;
    this.$init = function () {
      $lt21.$init.call(this);
      this.FDesc = false;
      this.FValue = undefined;
      this.FField = null;
      this.FOptions = {};
      this.FDataset = null;
    };
    this.$final = function () {
      this.FField = undefined;
      this.FOptions = undefined;
      this.FDataset = undefined;
      $lt21.$final.call(this);
    };
    this.Create$1 = function (aDataset, aField, aValue, aOptions) {
      this.FField = aField;
      this.FValue = aValue;
      this.FOptions = rtl.refSet(aOptions);
      this.FDataset = aDataset;
      return this;
    };
    this.GetFieldValue = function (RowIndex) {
      var Result = undefined;
      Result = this.FDataset.FFieldMapper.GetJSONDataForField$1(this.FField,this.FDataset.FRows[RowIndex]);
      return Result;
    };
    this.CompareRows = function (RowIndex1, RowIndex2) {
      var Result = 0;
      Result = this.Compare(RowIndex1,this.GetFieldValue(RowIndex2));
      return Result;
    };
    this.Compare$1 = function (RowIndex) {
      var Result = 0;
      Result = this.Compare(RowIndex,this.FValue);
      return Result;
    };
  });
  rtl.createClass(this,"TStringFieldComparer",$lt5,function () {
    $lt6 = this;
    this.Compare = function (RowIndex, aValue) {
      var Result = 0;
      var S1 = "";
      var S2 = "";
      S1 = "" + this.GetFieldValue(RowIndex);
      S2 = "" + aValue;
      if (1 in this.FOptions) S1 = $lp(S1,1,S2.length);
      if (0 in this.FOptions) {
        Result = $lp1(S1,S2)}
       else Result = $lp2(S1,S2);
      return Result;
    };
  });
  rtl.createClass(this,"TNativeIntFieldComparer",$lt5,function () {
    $lt7 = this;
    this.Compare = function (RowIndex, aValue) {
      var Result = 0;
      var I1 = 0;
      var I2 = 0;
      I1 = rtl.trunc(this.GetFieldValue(RowIndex));
      I2 = rtl.trunc(aValue);
      Result = I1 - I2;
      return Result;
    };
  });
  rtl.createClass(this,"TBooleanFieldComparer",$lt5,function () {
    $lt8 = this;
    this.Compare = function (RowIndex, aValue) {
      var Result = 0;
      var B1 = false;
      var B2 = false;
      B1 = !(this.GetFieldValue(RowIndex) == false);
      B2 = !(aValue == false);
      Result = (B1 + 0) - (B2 + 0);
      return Result;
    };
  });
  rtl.createClass(this,"TDateTimeFieldComparer",$lt5,function () {
    $lt9 = this;
    this.Compare = function (RowIndex, aValue) {
      var $Self = this;
      var Result = 0;
      var D1 = 0.0;
      var D2 = 0.0;
      function ToDate(v) {
        var Result = 0.0;
        if ($lp3(v)) {
          Result = $lp4(v)}
         else Result = $Self.FDataset.ConvertDateTimeField("" + v,$Self.FField);
        return Result;
      };
      D1 = ToDate(this.GetFieldValue(RowIndex));
      D2 = ToDate(aValue);
      Result = Math.round(D1 - D2);
      return Result;
    };
  });
  rtl.createClass(this,"TFloatFieldComparer",$lt5,function () {
    $lt10 = this;
    this.Compare = function (RowIndex, aValue) {
      var Result = 0;
      var D1 = 0.0;
      var D2 = 0.0;
      D1 = rtl.getNumber(this.GetFieldValue(RowIndex));
      D2 = rtl.getNumber(aValue);
      Result = Math.round(D1 - D2);
      return Result;
    };
  });
  rtl.createClass(this,"TRecordComparer",$lt21,function () {
    $lt11 = this;
    this.$init = function () {
      $lt21.$init.call(this);
      this.FDataset = null;
      this.FIndexBased = false;
      this.FItems = [];
      this.FOptions = {};
      this.FValues = [];
    };
    this.$final = function () {
      this.FDataset = undefined;
      this.FItems = undefined;
      this.FOptions = undefined;
      this.FValues = undefined;
      $lt21.$final.call(this);
    };
    this.ConstructItems = function (aFields) {
      var L = null;
      var FCC = null;
      var F = null;
      var I = 0;
      L = $lt25.$create("Create");
      try {
        this.FDataset.GetFieldList$1(L,aFields);
        if (!this.FIndexBased && (L.FCount !== rtl.length(this.FValues))) throw $lt26.$create("CreateFmt",["Array of values has different length (%d) from array of fields (%d)",$lp5(19,rtl.length(this.FValues),0,L.FCount)]);
        this.FItems = rtl.arraySetLength(this.FItems,null,L.FCount);
        for (var $l = 0, $end = L.FCount - 1; $l <= $end; $l++) {
          I = $l;
          F = rtl.getObject(L.Get(I));
          FCC = this.DataTypeToComparerClass(F.FDataType);
          if (FCC === null) throw $lt26.$create("CreateFmt",["Cannot locate on field %s of type %s)",$lp5(18,F.FFieldName,18,$lp6($lm1.$rtti["TFieldType"],F.FDataType))]);
          if (this.FIndexBased) {
            this.FItems[I] = FCC.$create("Create$1",[this.FDataset,F,null,rtl.refSet(this.FOptions)])}
           else this.FItems[I] = FCC.$create("Create$1",[this.FDataset,F,this.FValues[I],rtl.refSet(this.FOptions)]);
        };
      } finally {
        L = rtl.freeLoc(L);
      };
    };
    this.DataTypeToComparerClass = function (aFieldType) {
      var Result = null;
      var $tmp = aFieldType;
      if (($tmp === 11) || ($tmp === 12) || ($tmp === 1)) {
        Result = $lt6}
       else if (($tmp === 9) || ($tmp === 2) || ($tmp === 3)) {
        Result = $lt7}
       else if ($tmp === 4) {
        Result = $lt8}
       else if ($tmp === 5) {
        Result = $lt10}
       else if (($tmp === 6) || ($tmp === 7) || ($tmp === 8)) {
        Result = $lt9}
       else {
        Result = null;
      };
      return Result;
    };
    this.Compare = function (aRowindex) {
      var Result = 0;
      var I = 0;
      var L = 0;
      Result = 0;
      I = 0;
      L = rtl.length(this.FItems);
      while ((Result === 0) && (I < L)) {
        Result = this.FItems[I].Compare$1(aRowindex);
        I += 1;
      };
      return Result;
    };
    this.CompareRows = function (aRowindex1, aRowIndex2) {
      var Result = 0;
      var I = 0;
      var L = 0;
      Result = 0;
      I = 0;
      L = rtl.length(this.FItems);
      while ((Result === 0) && (I < L)) {
        Result = this.FItems[I].CompareRows(aRowindex1,aRowIndex2);
        if ((Result !== 0) && this.FItems[I].FDesc) Result = -Result;
        I += 1;
      };
      return Result;
    };
    this.updateFromIndex = function (aIndex) {
      var L = null;
      var I = 0;
      L = $lt25.$create("Create");
      try {
        if (aIndex.FCaseinsFields !== "") {
          this.FDataset.GetFieldList$1(L,aIndex.FCaseinsFields);
          for (var $l = 0, $end = rtl.length(this.FItems) - 1; $l <= $end; $l++) {
            I = $l;
            if (L.IndexOf(this.FItems[I].FField) !== -1) this.FItems[I].FOptions = rtl.unionSet(this.FItems[I].FOptions,rtl.createSet(0));
          };
        };
        L.Clear();
        this.FDataset.GetFieldList$1(L,aIndex.FDescFields);
        for (var $l1 = 0, $end1 = rtl.length(this.FItems) - 1; $l1 <= $end1; $l1++) {
          I = $l1;
          this.FItems[I].FDesc = (2 in aIndex.FOptions) || (L.IndexOf(this.FItems[I].FField) !== -1);
        };
      } finally {
        L = rtl.freeLoc(L);
      };
    };
    this.Create$1 = function (aDataset, aFields, aValues, aOptions) {
      this.FDataset = aDataset;
      if (rtl.isArray(aValues)) {
        this.FValues = aValues}
       else {
        this.FValues = rtl.arraySetLength(this.FValues,undefined,1);
        this.FValues[0] = aValues;
      };
      this.FOptions = rtl.refSet(aOptions);
      this.ConstructItems(aFields);
      return this;
    };
    this.Create$2 = function (aDataset, aIndex) {
      this.FDataset = aDataset;
      this.FIndexBased = true;
      if (3 in aIndex.FOptions) this.FOptions = rtl.createSet(0);
      this.ConstructItems(aIndex.FFields);
      this.updateFromIndex(aIndex);
      return this;
    };
    this.Destroy = function () {
      var I = 0;
      for (var $l = 0, $end = rtl.length(this.FItems) - 1; $l <= $end; $l++) {
        I = $l;
        rtl.free(this.FItems,I);
      };
      $lt21.Destroy.call(this);
    };
  });
  rtl.createClass(this,"TJSONIndex",$lt21,function () {
    $lt12 = this;
    this.$init = function () {
      $lt21.$init.call(this);
      this.FList = null;
      this.FRows = null;
      this.FDataset = null;
    };
    this.$final = function () {
      this.FList = undefined;
      this.FRows = undefined;
      this.FDataset = undefined;
      $lt21.$final.call(this);
    };
    this.GetRecordIndex = function (aListIndex) {
      var Result = 0;
      if ($lp7(this.FList[aListIndex])) {
        Result = -1}
       else Result = rtl.trunc(this.FList[aListIndex]);
      return Result;
    };
    this.GetCount = function () {
      var Result = 0;
      Result = this.FList.length;
      return Result;
    };
    this.ClearIndex = function () {
      this.FList.length = 0;
    };
    this.Create$1 = function (aDataset, aRows) {
      this.FRows = aRows;
      this.FList = new Array(this.FRows.length);
      this.FDataset = aDataset;
      this.CreateIndex();
      return this;
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      Result = this.Append(aRecordIndex);
      return Result;
    };
  });
  rtl.createClass(this,"TDefaultJSONIndex",$lt12,function () {
    $lt13 = this;
    this.CreateIndex = function () {
      var I = 0;
      for (var $l = 0, $end = this.FRows.length - 1; $l <= $end; $l++) {
        I = $l;
        this.FList[I] = I;
      };
    };
    this.AppendToIndex = function () {
      var I = 0;
      var L = 0;
      L = this.FList.length;
      this.FList.length = this.FRows.length;
      for (var $l = L, $end = this.FRows.length - 1; $l <= $end; $l++) {
        I = $l;
        this.FList[I] = I;
      };
    };
    this.Append = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.push(aRecordIndex) - 1;
      return Result;
    };
    this.Insert = function (aCurrentIndex, aRecordIndex) {
      var Result = 0;
      this.FList.splice(aCurrentIndex,0,aRecordIndex);
      Result = aCurrentIndex;
      return Result;
    };
    this.FindRecord = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.indexOf(aRecordIndex);
      return Result;
    };
    this.Update = function (aRecordIndex) {
      var Result = 0;
      Result = aRecordIndex;
      return Result;
    };
  });
  rtl.createClass(this,"TSortedJSONIndex",$lt12,function () {
    $lt14 = this;
    this.$init = function () {
      $lt12.$init.call(this);
      this.FComparer = null;
      this.FUnique = false;
    };
    this.$final = function () {
      this.FComparer = undefined;
      $lt12.$final.call(this);
    };
    this.FindPos = function (aRecordIndex) {
      var Result = 0;
      var L = 0;
      var R = 0;
      var I = 0;
      var CompareRes = 0;
      if (!(this.FComparer != null)) return Result;
      L = 0;
      R = this.GetCount() - 1;
      while (L <= R) {
        I = L + rtl.trunc((R - L) / 2);
        CompareRes = this.FComparer.CompareRows(aRecordIndex,rtl.trunc(this.FList[I]));
        if (CompareRes > 0) {
          L = I + 1}
         else {
          R = I - 1;
          if (CompareRes === 0) {
            if (this.FUnique) L = I;
          };
        };
      };
      Result = L;
      return Result;
    };
    this.MergeSort = function (aList) {
      var Result = null;
      var temp = null;
      var l = 0;
      var p = 0;
      var q = 0;
      var e = 0;
      var tail = 0;
      var insize = 0;
      var nmerges = 0;
      var psize = 0;
      var qsize = 0;
      if (aList === null) return null;
      l = aList.length;
      Result = new Array(l);
      if (l === 0) return Result;
      insize = 1;
      do {
        p = 0;
        tail = 0;
        nmerges = 0;
        while (p < l) {
          nmerges += 1;
          psize = l - p;
          if (insize < psize) psize = insize;
          q = p + psize;
          qsize = insize;
          while ((psize > 0) || ((qsize > 0) && (q < l))) {
            if (psize === 0) {
              e = q;
              q += 1;
              qsize -= 1;
            } else if ((qsize === 0) || (q >= l)) {
              e = p;
              p += 1;
              psize -= 1;
            } else if (this.FComparer.CompareRows(rtl.trunc(aList[p]),rtl.trunc(aList[q])) <= 0) {
              e = p;
              p += 1;
              psize -= 1;
            } else {
              e = q;
              q += 1;
              qsize -= 1;
            };
            Result[tail] = aList[e];
            tail += 1;
          };
          p = q;
        };
        if (nmerges <= 1) return Result;
        insize = insize * 2;
        temp = Result;
        Result = aList;
        aList = temp;
      } while (!false);
      return Result;
    };
    this.Destroy = function () {
      $lp8({p: this, get: function () {
          return this.p.FComparer;
        }, set: function (v) {
          this.p.FComparer = v;
        }});
      $lt21.Destroy.call(this);
    };
    this.CreateComparer = function (aIndex) {
      $lp8({p: this, get: function () {
          return this.p.FComparer;
        }, set: function (v) {
          this.p.FComparer = v;
        }});
      this.FComparer = $lt11.$create("Create$2",[this.FDataset,aIndex]);
    };
    this.CreateIndex = function () {
      var Lst = null;
      var Len = 0;
      if (this.FComparer === null) return;
      Len = this.FRows.length - 1;
      Lst = new Array(Len + 1);
      while (Len >= 0) {
        Lst[Len] = Len;
        Len -= 1;
      };
      this.FList = this.MergeSort(Lst);
    };
    this.AppendToIndex = function () {
      this.CreateIndex();
    };
    this.Append = function (aRecordIndex) {
      var Result = 0;
      Result = this.FindPos(aRecordIndex);
      this.FList.splice(Result,0,aRecordIndex);
      return Result;
    };
    this.FindRecord = function (aRecordIndex) {
      var Result = 0;
      Result = this.FList.indexOf(aRecordIndex);
      return Result;
    };
    this.Update = function (aRecordIndex) {
      var Result = 0;
      var aCurrentIndex = 0;
      aCurrentIndex = this.FindRecord(aRecordIndex);
      Result = this.FindPos(aRecordIndex);
      if (Result !== aCurrentIndex) this.FList.splice(Result,0,this.FList.splice(aCurrentIndex,1)[0]);
      return Result;
    };
  });
  rtl.createClass(this,"TJSONIndexDef",$lt27,function () {
    $lt15 = this;
    this.$init = function () {
      $lt27.$init.call(this);
      this.FIndex = null;
    };
    this.$final = function () {
      this.FIndex = undefined;
      $lt27.$final.call(this);
    };
    this.ClearIndex = function () {
      $lp8({p: this, get: function () {
          return this.p.FIndex;
        }, set: function (v) {
          this.p.FIndex = v;
        }});
    };
    this.BuildIndex = function (aDataset) {
      if (this.FIndex === null) {
        this.FIndex = $lt14.$create("Create$1",[aDataset,aDataset.FRows])}
       else this.FIndex.ClearIndex();
      this.FIndex.CreateComparer(this);
      this.FIndex.CreateIndex();
    };
  });
  rtl.createClass(this,"TJSONIndexDefs",$lt28,function () {
    $lt16 = this;
    this.GetD = function (aIndex) {
      var Result = null;
      Result = rtl.as(this.GetItem$1(aIndex),$lt15);
      return Result;
    };
  });
  rtl.createClass(this,"TBaseJSONDataSet",$lt29,function () {
    $lt17 = this;
    this.$init = function () {
      $lt29.$init.call(this);
      this.FActiveIndex = "";
      this.FIndexes = null;
      this.FMUS = false;
      this.FOwnsData = false;
      this.FDefaultIndex = null;
      this.FCurrentIndex = null;
      this.FCurrent = 0;
      this.FMetaData = null;
      this.FRows = null;
      this.FDeletedRows = null;
      this.FFieldMapper = null;
      this.FEditIdx = 0;
      this.FEditRow = undefined;
      this.FFilterRow = undefined;
      this.FUseDateTimeFormatFields = false;
      this.FRowType = 0;
      this.FFilterExpression = null;
    };
    this.$final = function () {
      this.FIndexes = undefined;
      this.FDefaultIndex = undefined;
      this.FCurrentIndex = undefined;
      this.FMetaData = undefined;
      this.FRows = undefined;
      this.FDeletedRows = undefined;
      this.FFieldMapper = undefined;
      this.FFilterExpression = undefined;
      $lt29.$final.call(this);
    };
    this.GetFilterField = function (AName) {
      var Result = $lt30.$new();
      var F = null;
      var C = 0;
      F = this.FieldByName(AName);
      Result.ResultType = this.FieldTypeToExpressionType(F.FDataType);
      var $tmp = Result.ResultType;
      if ($tmp === 0) {
        Result.resValue = F.GetAsBoolean()}
       else if ($tmp === 1) {
        Result.resValue = F.GetAsLargeInt()}
       else if ($tmp === 2) {
        Result.resValue = F.GetAsFloat()}
       else if ($tmp === 3) {
        Result.resValue = F.GetAsDateTime()}
       else if ($tmp === 4) {
        Result.resValue = F.GetAsString()}
       else if ($tmp === 5) {
        C = F.GetAsFloat() * 10000;
        Result.resValue = C / 10000;
      };
      return Result;
    };
    this.SetActiveIndex = function (AValue) {
      if (this.FActiveIndex === AValue) return;
      this.FActiveIndex = AValue;
      if (0 in this.FComponentState) return;
      this.ActivateIndex(this.GetActive());
    };
    this.SetIndexes = function (AValue) {
      if (this.FIndexes === AValue) return;
      this.FIndexes.Assign(AValue);
      if (this.GetActive()) this.BuildIndexes();
    };
    this.SetMetaData = function (AValue) {
      this.CheckInactive();
      this.FMetaData = AValue;
    };
    this.SetRows = function (AValue) {
      if (AValue === this.FRows) return;
      this.CheckInactive();
      this.FRows = null;
      this.AddToRows(AValue);
    };
    this.RemoveCalcFields = function (Buf) {
      var i = 0;
      for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        if (this.FFieldList.GetField(i).FFieldKind in rtl.createSet(1,3,2)) this.FFieldMapper.RemoveField(this.FFieldList.GetField(i).FFieldName,this.FFieldList.GetField(i).GetIndex(),Buf);
      };
    };
    this.ActivateIndex = function (Build) {
      var Idx = null;
      if (this.FActiveIndex !== "") {
        Idx = rtl.as(this.FIndexes.Find$1(this.FActiveIndex),$lt15)}
       else Idx = null;
      if (Idx === null) {
        this.FCurrentIndex = this.FDefaultIndex}
       else {
        if ((Idx.FIndex === null) && Build) Idx.BuildIndex(this);
        this.FCurrentIndex = Idx.FIndex;
      };
      if (this.GetActive()) this.Resync(rtl.createSet(1));
    };
    this.FieldTypeToExpressionType = function (aDataType) {
      var Result = 0;
      var $tmp = aDataType;
      if (($tmp === 11) || ($tmp === 12) || ($tmp === 1)) {
        Result = 4}
       else if (($tmp === 2) || ($tmp === 9) || ($tmp === 3)) {
        Result = 1}
       else if ($tmp === 4) {
        Result = 0}
       else if ($tmp === 5) {
        Result = 2}
       else if (($tmp === 6) || ($tmp === 7) || ($tmp === 8)) {
        Result = 3}
       else {
        $lp9("Fields of type %s are not supported in filter expressions.",$lp5(18,$lm1.Fieldtypenames[aDataType]),this);
      };
      return Result;
    };
    this.GetFilterIsNull = function (Args) {
      var Result = $lt30.$new();
      Result.ResultType = 0;
      Result.resValue = this.FieldByName("" + Args[0].resValue).GetIsNull();
      return Result;
    };
    this.FilterExpressionClass = function () {
      var Result = null;
      Result = $lt31;
      return Result;
    };
    this.CreateFilterExpression = function () {
      var Result = null;
      var I = 0;
      Result = this.FilterExpressionClass().$create("Create$1",[this]);
      for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        Result.FIdentifiers.AddVariable(this.FFieldList.GetField(I).FFieldName,this.FieldTypeToExpressionType(this.FFieldList.GetField(I).FDataType),rtl.createCallback(this,"GetFilterField"));
      };
      Result.FIdentifiers.AddFunction("IsNull","B","S",rtl.createCallback(this,"GetFilterIsNull"));
      Result.SetExpression(this.FFilterText);
      return Result;
    };
    this.DoFilterRecord = function () {
      var Result = false;
      var DS = 0;
      Result = true;
      DS = this.SetTempState(6);
      try {
        if (this.FOnFilterRecord != null) {
          this.FOnFilterRecord(this,{get: function () {
              return Result;
            }, set: function (v) {
              Result = v;
            }});
          if (!Result) return Result;
        };
        if (!this.FFiltered || (this.FFilterText === "")) return Result;
        if (this.FFilterExpression === null) this.FFilterExpression = this.CreateFilterExpression();
        Result = this.FFilterExpression.GetAsBoolean();
      } finally {
        this.RestoreState(DS);
      };
      return Result;
    };
    this.CreateIndexDefs = function () {
      var Result = null;
      Result = $lt16.$create("create$3",[this,this,$lt15]);
      return Result;
    };
    this.RecordComparerClass = function () {
      var Result = null;
      Result = $lt11;
      return Result;
    };
    this.LocateRecordIndex = function (KeyFields, KeyValues, Options) {
      var Result = 0;
      var Comp = null;
      var RI = 0;
      var I = 0;
      Result = -1;
      Comp = this.RecordComparerClass().$create("Create$1",[this,KeyFields,KeyValues,rtl.refSet(Options)]);
      try {
        if (2 in Options) {
          I = this.FCurrent}
         else I = 0;
        RI = this.FCurrentIndex.GetRecordIndex(I);
        while ((Result === -1) && (RI !== -1)) {
          if (Comp.Compare(RI) === 0) Result = RI;
          I += 1;
          RI = this.FCurrentIndex.GetRecordIndex(I);
        };
      } finally {
        Comp = rtl.freeLoc(Comp);
      };
      return Result;
    };
    this.AllocRecordBuffer = function () {
      var Result = $lt32.$new();
      Result.data = new Object();
      Result.bookmark = null;
      Result.state = 0;
      return Result;
    };
    this.FreeRecordBuffer = function (Buffer) {
      Buffer.data = null;
      Buffer.bookmark = null;
      Buffer.state = 0;
    };
    this.InternalInitRecord = function (Buffer) {
      Buffer.data = this.FFieldMapper.CreateRow();
      Buffer.bookmark = null;
      Buffer.state = 0;
    };
    this.GetRecord = function (Buffer, GetMode, DoCheck) {
      var Result = 0;
      var BkmIdx = 0;
      var recordAccepted = false;
      Result = 0;
      do {
        recordAccepted = true;
        var $tmp = GetMode;
        if ($tmp === 1) {
          if (this.FCurrent < (this.FCurrentIndex.GetCount() - 1)) {
            this.FCurrent += 1}
           else Result = 2}
         else if ($tmp === 2) {
          if (this.FCurrent > 0) {
            this.FCurrent -= 1}
           else Result = 1}
         else if ($tmp === 0) if ((this.FCurrent < 0) || (this.FCurrent >= this.FCurrentIndex.GetCount())) Result = 2;
        if (Result === 0) {
          BkmIdx = this.FCurrentIndex.GetRecordIndex(this.FCurrent);
          Buffer.data = this.FRows[BkmIdx];
          Buffer.bookmarkFlag = 0;
          Buffer.bookmark = BkmIdx;
          this.GetCalcFields(Buffer);
          if (this.FFiltered) {
            this.FFilterRow = Buffer.data;
            recordAccepted = this.DoFilterRecord();
            if (!recordAccepted && (GetMode === 0)) {
              recordAccepted = true;
              Result = 2;
            };
          };
        };
      } while (!recordAccepted);
      return Result;
    };
    this.AddToRows = function (AValue) {
      if (this.FRows === null) {
        this.FRows = AValue}
       else {
        this.FRows = this.FRows.concat(AValue);
        this.AppendToIndexes();
      };
    };
    this.InternalClose = function () {
      this.BindFields(false);
      if (this.FDefaultFields) this.DestroyFields();
      this.FreeData();
    };
    this.InternalFirst = function () {
      this.FCurrent = -1;
    };
    this.InternalLast = function () {
      this.FCurrent = this.FCurrentIndex.GetCount();
    };
    this.InternalOpen = function () {
      $lp8({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FFieldMapper = this.CreateFieldMapper();
      if (this.FRows === null) {
        this.FRows = new Array();
        this.FOwnsData = true;
      };
      this.CreateIndexes();
      this.InternalInitFieldDefs();
      if (this.FDefaultFields) this.CreateFields();
      this.BindFields(true);
      this.InitDateTimeFields();
      if (this.FActiveIndex !== "") this.ActivateIndex(true);
      this.FCurrent = -1;
    };
    this.InternalPost = function () {
      var I = 0;
      var NewIdx = 0;
      var NewCurrent = 0;
      var Idx = 0;
      var B = $lt33.$new();
      NewCurrent = -1;
      this.GetBookmarkData($lt32.$clone(this.ActiveBuffer()),B);
      if (this.FState === 3) {
        Idx = this.FRows.push(this.FEditRow) - 1;
        if (this.GetBookmarkFlag($lt32.$clone(this.ActiveBuffer())) === 2) {
          this.FDefaultIndex.Append(Idx);
          for (var $l = 0, $end = this.FIndexes.GetCount() - 1; $l <= $end; $l++) {
            I = $l;
            if (this.FIndexes.GetD(I).FIndex != null) {
              NewIdx = this.FIndexes.GetD(I).FIndex.Append(Idx);
              if (this.FIndexes.GetD(I).FIndex === this.FCurrentIndex) NewCurrent = NewIdx;
            };
          };
        } else {
          this.FCurrent = this.FDefaultIndex.Insert(this.FCurrent,Idx);
          for (var $l1 = 0, $end1 = this.FIndexes.GetCount() - 1; $l1 <= $end1; $l1++) {
            I = $l1;
            if (this.FIndexes.GetD(I).FIndex != null) {
              NewIdx = this.FIndexes.GetD(I).FIndex.Append(Idx);
              if (this.FIndexes.GetD(I).FIndex === this.FCurrentIndex) NewCurrent = NewIdx;
            };
          };
        };
      } else {
        if (this.FEditIdx === -1) $lp10("Failed to retrieve record index for record %d",$lp5(0,this.FCurrent));
        Idx = this.FEditIdx;
        this.FRows[Idx] = this.FEditRow;
        this.FDefaultIndex.Update(Idx);
        for (var $l2 = 0, $end2 = this.FIndexes.GetCount() - 1; $l2 <= $end2; $l2++) {
          I = $l2;
          NewIdx = this.FCurrentIndex.Update(Idx);
          if (this.FIndexes.GetD(I).FIndex != null) if (this.FIndexes.GetD(I).FIndex === this.FCurrentIndex) NewCurrent = NewIdx;
        };
      };
      if (NewCurrent !== -1) this.FCurrent = NewCurrent;
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalCancel = function () {
      this.FEditIdx = -1;
      this.FEditRow = null;
    };
    this.InternalInitFieldDefs = function () {
      if (this.FMetaData != null) this.MetaDataToFieldDefs();
      if (this.FFieldDefs.GetCount() === 0) throw $lt20.$create("Create$1",["No fields found"]);
    };
    this.InternalSetToRecord = function (Buffer) {
      this.FCurrent = this.FCurrentIndex.FindRecord(rtl.trunc(Buffer.bookmark));
    };
    this.SetFilterText = function (Value) {
      $lt29.SetFilterText.call(this,Value);
      $lp8({p: this, get: function () {
          return this.p.FFilterExpression;
        }, set: function (v) {
          this.p.FFilterExpression = v;
        }});
      if (this.GetActive()) this.Resync(rtl.createSet(1));
    };
    this.SetFiltered = function (Value) {
      $lt29.SetFiltered.call(this,Value);
      $lp8({p: this, get: function () {
          return this.p.FFilterExpression;
        }, set: function (v) {
          this.p.FFilterExpression = v;
        }});
      if (this.GetActive()) this.Resync(rtl.createSet(1));
    };
    this.GetFieldClass = function (FieldType) {
      var Result = null;
      if (this.FUseDateTimeFormatFields && (FieldType in rtl.createSet(6,8,7))) {
        var $tmp = FieldType;
        if ($tmp === 6) {
          Result = $lt2}
         else if ($tmp === 8) {
          Result = $lt4}
         else if ($tmp === 7) Result = $lt3;
      } else Result = $lt29.GetFieldClass.call(this,FieldType);
      return Result;
    };
    this.GetApplyUpdateData = function (Buffer) {
      var Result = undefined;
      Result = this.FFieldMapper.CopyRow(Buffer.data);
      this.RemoveCalcFields(Result);
      return Result;
    };
    this.IsCursorOpen = function () {
      var Result = false;
      Result = this.FDefaultIndex != null;
      return Result;
    };
    this.GetBookmarkData = function (Buffer, Data) {
      Data.Data = Buffer.bookmark;
    };
    this.GetBookmarkFlag = function (Buffer) {
      var Result = 0;
      Result = Buffer.bookmarkFlag;
      return Result;
    };
    this.BuildIndexes = function () {
      var I = 0;
      for (var $l = 0, $end = this.FIndexes.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        this.FIndexes.GetD(I).BuildIndex(this);
      };
    };
    this.FreeData = function () {
      var I = 0;
      if (this.FOwnsData) {
        this.FRows = null;
        this.FMetaData = null;
      };
      for (var $l = 0, $end = this.FIndexes.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        this.FIndexes.GetD(I).ClearIndex();
      };
      this.FCurrentIndex = null;
      $lp8({p: this, get: function () {
          return this.p.FDefaultIndex;
        }, set: function (v) {
          this.p.FDefaultIndex = v;
        }});
      $lp8({p: this, get: function () {
          return this.p.FFieldMapper;
        }, set: function (v) {
          this.p.FFieldMapper = v;
        }});
      this.FCurrentIndex = null;
      this.FDeletedRows = null;
    };
    this.AppendToIndexes = function () {
      this.FDefaultIndex.AppendToIndex();
      if ((this.FCurrentIndex != null) && (this.FCurrentIndex !== this.FDefaultIndex)) this.FCurrentIndex.AppendToIndex();
    };
    this.CreateIndexes = function () {
      this.FDefaultIndex = $lt13.$create("Create$1",[this,this.FRows]);
      this.AppendToIndexes();
      if (this.FCurrentIndex === null) this.FCurrentIndex = this.FDefaultIndex;
    };
    this.InitDateTimeFields = function () {
    };
    this.ConvertDateTimeField = function (S, F) {
      var Result = 0.0;
      var Ptrn = "";
      Result = 0;
      Ptrn = "";
      var $tmp = F.FDataType;
      if ($tmp === 6) {
        if ($lt2.isPrototypeOf(F)) Ptrn = rtl.as(F,$lt2).FDateFormat}
       else if ($tmp === 7) {
        if ($lt3.isPrototypeOf(F)) Ptrn = rtl.as(F,$lt3).FTimeFormat}
       else if ($tmp === 8) if ($lt4.isPrototypeOf(F)) Ptrn = rtl.as(F,$lt4).FDateTimeFormat;
      if (Ptrn === "") {
        Result = this.$class.DefaultConvertToDateTime(F,S,true)}
       else Result = $lp11(Ptrn,S,1);
      return Result;
    };
    this.FormatDateTimeField = function (DT, F) {
      var Result = "";
      var Ptrn = "";
      Result = "";
      Ptrn = "";
      var $tmp = F.FDataType;
      if ($tmp === 6) {
        if ($lt2.isPrototypeOf(F)) Ptrn = F.FDateFormat}
       else if ($tmp === 7) {
        if ($lt3.isPrototypeOf(F)) Ptrn = F.FTimeFormat}
       else if ($tmp === 8) if ($lt4.isPrototypeOf(F)) Ptrn = F.FDateTimeFormat;
      if (Ptrn === "") {
        Result = $lp12(DT)}
       else Result = $lp13(Ptrn,DT);
      return Result;
    };
    this.CreateFieldMapper = function () {
      var Result = null;
      if (this.FRowType === 1) {
        Result = $lt19.$create("Create")}
       else Result = $lt18.$create("Create");
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt29.Create$1.apply(this,arguments);
      this.FOwnsData = true;
      this.FUseDateTimeFormatFields = false;
      this.FEditIdx = -1;
      this.FIndexes = this.CreateIndexDefs();
      return this;
    };
    this.Destroy = function () {
      this.Close();
      $lp8({p: this, get: function () {
          return this.p.FFilterExpression;
        }, set: function (v) {
          this.p.FFilterExpression = v;
        }});
      $lp8({p: this, get: function () {
          return this.p.FIndexes;
        }, set: function (v) {
          this.p.FIndexes = v;
        }});
      this.FEditIdx = -1;
      $lt29.Destroy.call(this);
    };
    this.ConvertDateTimeToNative = function (aField, aValue) {
      var Result = undefined;
      if (isNaN(aValue)) {
        Result = null}
       else Result = this.FormatDateTimeField(aValue,aField);
      return Result;
    };
    this.Lookup = function (KeyFields, KeyValues, ResultFields) {
      var Result = undefined;
      var RI = 0;
      var I = 0;
      var l = null;
      var Vals = [];
      Result = null;
      l = $lt25.$create("Create");
      try {
        this.GetFieldList$1(l,ResultFields);
        Result = $lt29.Lookup.call(this,KeyFields,KeyValues,ResultFields);
        RI = this.LocateRecordIndex(KeyFields,KeyValues,{});
        if (RI !== -1) {
          Vals = rtl.arraySetLength(Vals,undefined,l.FCount);
          for (var $l = 0, $end = l.FCount - 1; $l <= $end; $l++) {
            I = $l;
            Vals[I] = this.FFieldMapper.GetJSONDataForField$1(rtl.getObject(l.Get(I)),this.FRows[RI]);
          };
          if (l.FCount === 1) {
            Result = Vals[I]}
           else Result = Vals;
        };
      } finally {
        l = rtl.freeLoc(l);
      };
      return Result;
    };
    this.GetFieldData$1 = function (Field, Buffer) {
      var Result = undefined;
      var R = undefined;
      if (this.FState in rtl.createSet(5,11)) {
        R = this.FCalcBuffer.data}
       else if (this.FState === 6) {
        R = this.FFilterRow}
       else if (this.FEditIdx == Buffer.bookmark) {
        if (this.FState === 8) {
          R = Buffer.data}
         else R = this.FEditRow;
      } else {
        if (this.FState === 8) {
          return null}
         else R = Buffer.data;
      };
      Result = this.FFieldMapper.GetJSONDataForField$1(Field,R);
      if ($lp7(Result)) Result = null;
      return Result;
    };
    this.SetFieldData$1 = function (Field, Buffer, AValue) {
      var R = undefined;
      if (this.FState in rtl.createSet(5,11)) {
        R = this.FCalcBuffer.data}
       else R = this.FEditRow;
      this.FFieldMapper.SetJSONDataForField$1(Field,R,AValue);
      if (!(this.FState in rtl.createSet(5,11,6,7))) this.DataEvent(0,Field);
      this.SetModified(true);
    };
    this.CompareBookmarks = function (Bookmark1, Bookmark2) {
      var Result = 0;
      if (rtl.isNumber(Bookmark1.Data) && rtl.isNumber(Bookmark2.Data)) {
        Result = rtl.trunc(Bookmark2.Data) - rtl.trunc(Bookmark1.Data)}
       else {
        if (rtl.isNumber(Bookmark1.Data)) {
          Result = -1}
         else if (rtl.isNumber(Bookmark2.Data)) {
          Result = 1}
         else Result = 0;
      };
      return Result;
    };
  });
  rtl.createClass(this,"TJSONObjectFieldMapper",$lt1,function () {
    $lt18 = this;
    this.RemoveField = function (FieldName, FieldIndex, Row) {
      $lp14(Row,FieldName);
    };
    this.SetJSONDataForField = function (FieldName, FieldIndex, Row, Data) {
      Row[FieldName] = Data;
    };
    this.GetJSONDataForField = function (FieldName, FieldIndex, Row) {
      var Result = undefined;
      Result = Row[FieldName];
      return Result;
    };
    this.CreateRow = function () {
      var Result = undefined;
      Result = new Object();
      return Result;
    };
  });
  rtl.createClass(this,"TJSONArrayFieldMapper",$lt1,function () {
    $lt19 = this;
    this.RemoveField = function (FieldName, FieldIndex, Row) {
      Row.splice(FieldIndex,1);
    };
    this.SetJSONDataForField = function (FieldName, FieldIndex, Row, Data) {
      Row[FieldIndex] = Data;
    };
    this.GetJSONDataForField = function (FieldName, FieldIndex, Row) {
      var Result = undefined;
      Result = Row[FieldIndex];
      return Result;
    };
    this.CreateRow = function () {
      var Result = undefined;
      Result = new Array();
      return Result;
    };
  });
  rtl.createClass(this,"EJSONDataset",$lt26,function () {
    $lt20 = this;
  });
  $mod.$implcode = function () {
    $lm7 = pas.DateUtils;
    $lp11 = $lm7.ScanDateTime;
    $lp12 = $lm7.DateTimeToRFC3339;
  };
},["DateUtils"]);
rtl.module("ExtJSDataset",["System","Classes","SysUtils","DB","JS","JSONDataset"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lm = pas.JSONDataset;
  var $lt1 = $lm.TBaseJSONDataSet;
  var $lm1 = pas.SysUtils;
  var $lp = $lm1.SameText;
  var $lm2 = pas.JS;
  var $lp1 = $lm2.isNull;
  var $lm3 = pas.DB;
  var $lp2 = $lm3.DatabaseError;
  var $lp3 = $lm1.StringReplace;
  var $lp4 = $lm1.LowerCase;
  var $lt2 = $lm.EJSONDataset;
  var $lm4 = pas.System;
  var $lp5 = $lm4.VarRecs;
  var $lp6 = $lm2.isUndefined;
  var $lt3 = $lm.TJSONDateField;
  var $lt4 = $lm.TJSONTimeField;
  var $lt5 = $lm.TJSONDateTimeField;
  var $lp7 = $lm2.toNumber;
  var $lp8 = $lm4.Trunc;
  rtl.createClass(this,"TExtJSJSONDataSet",$lt1,function () {
    $lt = this;
    this.$init = function () {
      $lt1.$init.call(this);
      this.FFields = null;
      this.FIDField = "";
      this.FRoot = "";
    };
    this.$final = function () {
      this.FFields = undefined;
      $lt1.$final.call(this);
    };
    this.InternalOpen = function () {
      var I = 0;
      $lt1.InternalOpen.call(this);
      for (var $l = 0, $end = this.FFieldList.GetCount() - 1; $l <= $end; $l++) {
        I = $l;
        if ($lp(this.FFieldList.GetField(I).FFieldName,this.FIDField)) this.FFieldList.GetField(I).FProviderFlags = rtl.unionSet(this.FFieldList.GetField(I).FProviderFlags,rtl.createSet(2));
      };
    };
    this.DataPacketReceived = function (ARequest) {
      var Result = false;
      var O = null;
      var A = null;
      Result = false;
      if ($lp1(ARequest.FData)) return Result;
      if (rtl.isString(ARequest.FData)) {
        O = JSON.parse("" + ARequest.FData)}
       else if (rtl.isObject(ARequest.FData)) {
        O = ARequest.FData}
       else $lp2("Cannot handle data packet");
      if (this.FRoot === "") this.FRoot = "rows";
      if (this.FIDField === "") this.FIDField = "id";
      if (O.hasOwnProperty("metaData") && rtl.isObject(O["metaData"])) {
        if (!this.GetActive()) this.SetMetaData(O["metaData"]);
        if (this.FMetaData.hasOwnProperty("root") && rtl.isString(this.FMetaData["root"])) this.FRoot = "" + this.FMetaData["root"];
        if (this.FMetaData.hasOwnProperty("idField") && rtl.isString(this.FMetaData["idField"])) this.FIDField = "" + this.FMetaData["idField"];
      };
      if (O.hasOwnProperty(this.FRoot) && rtl.isArray(O[this.FRoot])) {
        A = O[this.FRoot];
        Result = A.length > 0;
        this.AddToRows(A);
      };
      return Result;
    };
    this.ConvertDateFormat = function (S) {
      var Result = "";
      Result = $lp3(S,"y","yy",rtl.createSet(0));
      Result = $lp3(Result,"Y","yyyy",rtl.createSet(0));
      Result = $lp3(Result,"g","h",rtl.createSet(0));
      Result = $lp3(Result,"G","hh",rtl.createSet(0));
      Result = $lp3(Result,"F","mmmm",rtl.createSet(0));
      Result = $lp3(Result,"M","mmm",rtl.createSet(0));
      Result = $lp3(Result,"n","m",rtl.createSet(0));
      Result = $lp3(Result,"D","ddd",rtl.createSet(0));
      Result = $lp3(Result,"j","d",rtl.createSet(0));
      Result = $lp3(Result,"l","dddd",rtl.createSet(0));
      Result = $lp3(Result,"i","nn",rtl.createSet(0));
      Result = $lp3(Result,"u","zzz",rtl.createSet(0));
      Result = $lp3(Result,"a","am\/pm",rtl.createSet(0,1));
      Result = $lp4(Result);
      return Result;
    };
    this.MetaDataToFieldDefs = function () {
      var A = null;
      var F = null;
      var I = 0;
      var FS = 0;
      var N = "";
      var ft = 0;
      var D = undefined;
      this.FFieldDefs.Clear();
      D = this.FMetaData["fields"];
      if (!rtl.isArray(D)) throw $lt2.$create("Create$1",["Invalid metadata object"]);
      A = D;
      for (var $l = 0, $end = A.length - 1; $l <= $end; $l++) {
        I = $l;
        if (!rtl.isObject(A[I])) throw $lt2.$create("CreateFmt",["Field definition %d in metadata is not an object",$lp5(0,I)]);
        F = A[I];
        D = F["name"];
        if (!rtl.isString(D)) throw $lt2.$create("CreateFmt",["Field definition %d in has no or invalid name property",$lp5(0,I)]);
        N = "" + D;
        D = F["type"];
        if ($lp1(D) || $lp6(D)) {
          ft = 1}
         else if (!rtl.isString(D)) {
          throw $lt2.$create("CreateFmt",["Field definition %d in has invalid type property",$lp5(0,I)]);
        } else {
          ft = this.StringToFieldType("" + D);
        };
        if (ft === 1) {
          FS = this.GetStringFieldLength(F,N,I)}
         else FS = 0;
        this.FFieldDefs.Add$4(N,ft,FS);
      };
      this.FFields = A;
    };
    this.InitDateTimeFields = function () {
      var F = null;
      var FF = null;
      var I = 0;
      var Fmt = "";
      var D = undefined;
      if (this.FFields === null) return;
      for (var $l = 0, $end = this.FFields.length - 1; $l <= $end; $l++) {
        I = $l;
        F = this.FFields[I];
        D = F["type"];
        if (rtl.isString(D) && (("" + D) === "date")) {
          D = F["dateFormat"];
          if (rtl.isString(D)) {
            Fmt = this.ConvertDateFormat("" + D);
            FF = this.FindField("" + F["name"]);
            if ((FF !== null) && (FF.FDataType in rtl.createSet(6,7,8)) && (FF.FFieldKind === 0)) {
              if ($lt3.isPrototypeOf(FF)) {
                FF.FDateFormat = Fmt}
               else if ($lt4.isPrototypeOf(FF)) {
                FF.FTimeFormat = Fmt}
               else if ($lt5.isPrototypeOf(FF)) FF.FDateTimeFormat = Fmt;
            };
          };
        };
      };
    };
    this.StringToFieldType = function (S) {
      var Result = 0;
      if (S === "int") {
        Result = 3}
       else if (S === "float") {
        Result = 5}
       else if (S === "boolean") {
        Result = 4}
       else if (S === "date") {
        Result = 8}
       else if ((S === "string") || (S === "auto") || (S === "")) {
        Result = 1}
       else if (this.FMUS) {
        Result = 1}
       else throw $lt2.$create("CreateFmt",["Unknown JSON data type : %s",$lp5(18,S)]);
      return Result;
    };
    this.GetStringFieldLength = function (F, AName, AIndex) {
      var Result = 0;
      var I = 0;
      var L = 0;
      var D = undefined;
      Result = 0;
      D = F["maxlen"];
      if (!isNaN($lp7(D))) {
        Result = $lp8($lp7(D));
        if (Result <= 0) throw $lt2.$create("CreateFmt",["Invalid maximum length specifier for field %s",$lp5(18,AName)]);
      } else {
        for (var $l = 0, $end = this.FRows.length - 1; $l <= $end; $l++) {
          I = $l;
          D = this.FFieldMapper.GetJSONDataForField(AName,AIndex,this.FRows[I]);
          if (rtl.isString(D)) {
            L = ("" + D).length;
            if (L > Result) Result = L;
          };
        };
      };
      if (Result === 0) Result = 20;
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt1.Create$1.call(this,AOwner);
      this.FUseDateTimeFormatFields = true;
      return this;
    };
    var $r = this.$rtti;
    $r.addProperty("FieldDefs",2,$lm3.$rtti["TFieldDefs"],"FFieldDefs","SetFieldDefs");
    $r.addProperty("Indexes",2,$lm.$rtti["TJSONIndexDefs"],"FIndexes","SetIndexes");
    $r.addProperty("ActiveIndex",2,rtl.string,"FActiveIndex","SetActiveIndex");
    $r.addProperty("Active",3,rtl.boolean,"GetActive","SetActive",{Default: false});
    $r.addProperty("BeforeOpen",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeOpen","FBeforeOpen");
    $r.addProperty("AfterOpen",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterOpen","FAfterOpen");
    $r.addProperty("BeforeClose",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeClose","FBeforeClose");
    $r.addProperty("AfterClose",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterClose","FAfterClose");
    $r.addProperty("BeforeInsert",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeInsert","FBeforeInsert");
    $r.addProperty("AfterInsert",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterInsert","FAfterInsert");
    $r.addProperty("BeforeEdit",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeEdit","FBeforeEdit");
    $r.addProperty("AfterEdit",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterEdit","FAfterEdit");
    $r.addProperty("BeforePost",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforePost","FBeforePost");
    $r.addProperty("AfterPost",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterPost","FAfterPost");
    $r.addProperty("BeforeCancel",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeCancel","FBeforeCancel");
    $r.addProperty("AfterCancel",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterCancel","FAfterCancel");
    $r.addProperty("BeforeDelete",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeDelete","FBeforeDelete");
    $r.addProperty("AfterDelete",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterDelete","FAfterDelete");
    $r.addProperty("BeforeScroll",0,$lm3.$rtti["TDataSetNotifyEvent"],"FBeforeScroll","FBeforeScroll");
    $r.addProperty("AfterScroll",0,$lm3.$rtti["TDataSetNotifyEvent"],"FAfterScroll","FAfterScroll");
    $r.addProperty("OnCalcFields",0,$lm3.$rtti["TDataSetNotifyEvent"],"FOnCalcFields","FOnCalcFields");
    $r.addProperty("OnDeleteError",0,$lm3.$rtti["TDataSetErrorEvent"],"FOnDeleteError","FOnDeleteError");
    $r.addProperty("OnEditError",0,$lm3.$rtti["TDataSetErrorEvent"],"FOnEditError","FOnEditError");
    $r.addProperty("OnFilterRecord",2,$lm3.$rtti["TFilterRecordEvent"],"FOnFilterRecord","SetOnFilterRecord");
    $r.addProperty("OnNewRecord",0,$lm3.$rtti["TDataSetNotifyEvent"],"FOnNewRecord","FOnNewRecord");
    $r.addProperty("OnPostError",0,$lm3.$rtti["TDataSetErrorEvent"],"FOnPostError","FOnPostError");
    $r.addProperty("OwnsData",0,rtl.boolean,"FOwnsData","FOwnsData");
  });
});
rtl.module("ajaxlib",["System","Classes","SysUtils","Web","Types","JS"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lm = pas.Classes;
  var $lt1 = $lm.TComponent;
  var $lt2 = $lm.TStringList;
  rtl.createClass(this,"TWebRequest",$lt1,function () {
    $lt = this;
    this.$init = function () {
      $lt1.$init.call(this);
      this.FMethod = "";
      this.FURI = "";
      this.FCallback = null;
      this.FRequest = null;
      this.FHeaders = null;
    };
    this.$final = function () {
      this.FCallback = undefined;
      this.FRequest = undefined;
      this.FHeaders = undefined;
      $lt1.$final.call(this);
    };
    this.GetComplete = function () {
      var Result = false;
      if (this.FRequest.readyState === 4) {
        Result = true}
       else Result = false;
      return Result;
    };
    this.GetResponseText = function () {
      var Result = "";
      Result = this.FRequest.responseText;
      return Result;
    };
    this.GetStatus = function () {
      var Result = 0;
      Result = this.FRequest.status;
      return Result;
    };
    this.SetCallback = function (AValue) {
      if (rtl.eqCallback(this.FCallback,AValue)) return;
      this.FCallback = AValue;
      this.FRequest.onreadystatechange = AValue;
    };
    this.ProcessHeaders = function () {
      var i = 0;
      for (var $l = 0, $end = this.FHeaders.GetCount() - 1; $l <= $end; $l++) {
        i = $l;
        this.FRequest.setRequestHeader(this.FHeaders.GetName(i),this.FHeaders.GetValueFromIndex(i));
      };
    };
    this.Create$2 = function (AOwner, AMethod, AURI) {
      this.FRequest = new XMLHttpRequest();
      this.FHeaders = $lt2.$create("Create$1");
      this.FMethod = AMethod;
      this.FURI = AURI;
      return this;
    };
    this.DoRequest = function () {
      this.DoRequest$1("");
    };
    this.DoRequest$1 = function (body) {
      var uri = "";
      if (this.FMethod === "get") {
        uri = this.FURI + "?" + body}
       else uri = this.FURI;
      this.FRequest.open(this.FMethod,uri,true);
      this.ProcessHeaders();
      if (this.FMethod === "post") {
        this.FRequest.setRequestHeader("content-type","application\/x-www-form-urlencoded");
        if (body !== "") {
          this.FRequest.send(body);
          this.FHeaders.Clear();
          return;
        };
      };
      this.FRequest.send();
      this.FHeaders.Clear();
    };
  });
});
rtl.module("jsontable",["System","Classes","SysUtils","Web","ExtJSDataset","JS","ajaxlib"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  var $lt2 = null;
  var $lt3 = null;
  var $lt4 = null;
  var $lt5 = null;
  var $lm = pas.SysUtils;
  var $lt6 = $lm.Exception;
  var $lm1 = pas.Classes;
  var $lt7 = $lm1.TComponent;
  var $lm2 = pas.ExtJSDataset;
  var $lt8 = $lm2.TExtJSJSONDataSet;
  var $lm3 = pas.ajaxlib;
  var $lt9 = $lm3.TWebRequest;
  rtl.createClass(this,"EJSONTable",$lt6,function () {
    $lt = this;
  });
  rtl.createClass(this,"EBadDatafile",$lt,function () {
    $lt1 = this;
  });
  rtl.createClass(this,"ENoDataSet",$lt,function () {
    $lt2 = this;
  });
  rtl.createClass(this,"EBadTable",$lt,function () {
    $lt3 = this;
  });
  rtl.createClass(this,"TJSONTable",$lt7,function () {
    $lt4 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FActive = false;
      this.FDataSet = null;
      this.FDatafile = "";
      this.FFilter = "";
    };
    this.$final = function () {
      this.FDataSet = undefined;
      $lt7.$final.call(this);
    };
    this.GetDataSet = function () {
      var Result = null;
      if (!this.FActive) throw $lt2.$create("Create$1",["Dataset not available."]);
      Result = this.FDataSet;
      return Result;
    };
    this.GetInts = function (AField) {
      var Result = 0;
      Result = this.IntField(AField);
      return Result;
    };
    this.GetStrings = function (AField) {
      var Result = "";
      Result = this.StringField(AField);
      return Result;
    };
    this.SetFilter = function (AValue) {
      if (this.FFilter === AValue) return;
      if (!this.FActive) throw $lt2.$create("Create$1",["Dataset is not active."]);
      if (AValue === "") {
        this.FDataSet.SetFiltered(false)}
       else {
        this.FDataSet.SetFilterText(AValue);
        this.FDataSet.SetFiltered(true);
        this.FDataSet.First();
      };
      this.FFilter = AValue;
    };
    this.StringField = function (AField) {
      var Result = "";
      Result = this.FDataSet.FieldByName(AField).GetAsString();
      return Result;
    };
    this.IntField = function (AField) {
      var Result = 0;
      Result = this.FDataSet.FieldByName(AField).GetAsInteger();
      return Result;
    };
    this.Create$1 = function (AOwner) {
      $lt7.Create$1.call(this,AOwner);
      this.FDataSet = $lt8.$create("Create$1",[this]);
      this.FDatafile = "";
      return this;
    };
    this.Destroy = function () {
      rtl.free(this,"FDataSet");
      $lt7.Destroy.call(this);
    };
  });
  rtl.createClass(this,"TJSONDatabase",$lt7,function () {
    $lt5 = this;
    this.$init = function () {
      $lt7.$init.call(this);
      this.FActive = false;
      this.FJSON = null;
      this.FOnFailure = null;
      this.FOnSuccess = null;
      this.FRequest = null;
      this.FDatafile = "";
    };
    this.$final = function () {
      this.FJSON = undefined;
      this.FOnFailure = undefined;
      this.FOnSuccess = undefined;
      this.FRequest = undefined;
      $lt7.$final.call(this);
    };
    this.GetTable = function (ATable) {
      var Result = null;
      var tbl = null;
      if (!this.GetTables().includes(ATable)) throw $lt3.$create("Create$1",["Table does not exist: " + ATable]);
      tbl = this.FJSON[ATable];
      Result = $lt4.$create("Create$1",[this]);
      var $with = Result.FDataSet;
      $with.SetMetaData(tbl["metaData"]);
      $with.SetRows(tbl["Data"]);
      $with.Open();
      Result.FActive = true;
      return Result;
    };
    this.GetTables = function () {
      var Result = null;
      Result = this.FJSON["tables"];
      return Result;
    };
    this.SetActive = function (AValue) {
      if (this.FActive === AValue) return;
      if (AValue && (this.FDatafile === "")) throw $lt1.$create("Create$1",["Datafile is not set."]);
      if (AValue) {
        this.FRequest = $lt9.$create("Create$2",[this,"get",this.FDatafile + ".json"]);
        this.FRequest.SetCallback(rtl.createSafeCallback(this,"parseJSON"));
        this.FRequest.DoRequest();
      };
      this.FActive = AValue;
    };
    this.parseJSON = function () {
      if (!this.FRequest.GetComplete()) return;
      if (this.FRequest.GetStatus() !== 200) {
        if (this.FOnFailure != null) this.FOnFailure();
      } else {
        this.ParseDatabase(this.FRequest.GetResponseText());
        if (this.FOnSuccess != null) this.FOnSuccess();
      };
    };
    this.ParseDatabase = function (data) {
      this.FJSON = JSON.parse(data);
    };
    this.Create$1 = function (AOwner) {
      $lt7.Create$1.call(this,AOwner);
      this.FDatafile = "";
      return this;
    };
    this.Destroy = function () {
      $lt7.Destroy.call(this);
    };
  });
});
rtl.module("program",["System","browserconsole","browserapp","JS","Classes","SysUtils","Web","webrouter","jsontable"],function () {
  "use strict";
  var $mod = this;
  var $lt = null;
  var $lt1 = null;
  var $lm = pas.browserapp;
  var $lt2 = $lm.TBrowserApplication;
  var $lm1 = pas.SysUtils;
  var $lp = $lm1.IntToStr;
  var $lm2 = pas.webrouter;
  var $lp1 = $lm2.Router;
  var $lm3 = pas.jsontable;
  var $lt3 = $lm3.TJSONDatabase;
  $lt = this.TAppPage = {"0": "apNone", apNone: 0, "1": "apHome", apHome: 1, "2": "apMember", apMember: 2, "3": "apLot", apLot: 3};
  rtl.createClass(this,"TMyApplication",$lt2,function () {
    $lt1 = this;
    this.$init = function () {
      $lt2.$init.call(this);
      this.FSimsDB = null;
      this.FSimDB = null;
      this.FLotDB = null;
      this.FFamilyDB = null;
      this.FPage = 0;
    };
    this.$final = function () {
      this.FSimsDB = undefined;
      this.FSimDB = undefined;
      this.FLotDB = undefined;
      this.FFamilyDB = undefined;
      $lt2.$final.call(this);
    };
    this.ShowLayer = function (Id) {
      this.GetHTMLElement(Id).hidden = false;
    };
    this.HideLayer = function (Id) {
      this.GetHTMLElement(Id).hidden = true;
    };
    this.SetContent = function (Id, content) {
      this.GetHTMLElement(Id).innerHTML = content;
    };
    this.HideAllLayers = function () {
      var i = 0;
      for (i = 0; i <= 21; i++) this.HideLayer($mod.ResetLayers[i]);
      for (i = 1; i <= 4; i++) {
        this.HideLayer("FamilyMember" + $lp(i));
        this.HideLayer("FamilyMemberShadow" + $lp(i));
        this.HideLayer("FamilyMemberName" + $lp(i));
        if (i < 4) {
          this.HideLayer("FloorNav" + $lp(i));
        };
      };
    };
    this.PadMembersTable = function () {
      var tr = null;
      var td = null;
      var img = null;
      var tbl = null;
      tbl = this.GetHTMLElement("MembersTable");
      tr = tbl.insertRow(0);
      td = tr.insertCell(0);
      td.setAttribute("bgcolor","#EEEEDD");
      img = new Image();
      img.src = "s\/border.gif";
      img.setAttribute("vspace","0");
      img.setAttribute("hspace","0");
      img.height = 100;
      img.width = 1;
      img.setAttribute("border","0");
      img.setAttribute("align","left");
      td.appendChild(img);
      td = tr.insertCell(1);
      td.setAttribute("bgcolor","#EEEEDD");
      td.setAttribute("width","175");
      td.innerHTML = "<br\/>";
      td = tr.insertCell(2);
      td.setAttribute("bgcolor","#EEEEDD");
      img = new Image();
      img.src = "s\/border.gif";
      img.setAttribute("vspace","0");
      img.setAttribute("hspace","0");
      img.height = 100;
      img.width = 1;
      img.setAttribute("border","0");
      img.setAttribute("align","left");
      td.appendChild(img);
    };
    this.ShowFamilyHome = function (aEvent) {
      var Result = false;
      var i = 0;
      if (this.FPage !== 1) {
        Result = true;
        return Result;
      };
      this.HideAllLayers();
      this.AdjustCenterBox(365,"familyhomepagetxt");
      for (i = 0; i <= 7; i++) this.ShowLayer($mod.FamilyLayers[i]);
      for (var $l = 1, $end = this.FFamilyDB.GetInts("Members"); $l <= $end; $l++) {
        i = $l;
        this.ShowLayer("FamilyMember" + $lp(i));
        this.ShowLayer("FamilyMemberShadow" + $lp(i));
        this.ShowLayer("FamilyMemberName" + $lp(i));
      };
      Result = false;
      return Result;
    };
    this.ShowFamilyLot = function (aEvent) {
      var Result = false;
      var i = 0;
      this.FLotDB.SetFilter("ID=" + $lp(this.FFamilyDB.GetInts("Lot")));
      if (this.FLotDB.GetDataSet().FEOF) {
        window.alert("Invalid!");
        return Result;
      };
      this.GetHTMLElement("HouseImage").src = this.GetFamilyDir() + "house-exterior.jpg";
      var $with = this.FLotDB;
      document.title = "Number " + $with.GetStrings("Name") + ":";
      this.SetContent("FloorText",$with.GetStrings("Name") + ": Exterior");
      this.SetContent("HouseValue",$lp($with.GetInts("Value")));
      this.SetContent("HouseSize",$lp($with.GetInts("Size")));
      this.SetContent("HouseFloors",$lp($with.GetInts("Floors")));
      this.SetContent("HouseBaths",$lp($with.GetInts("Baths")));
      this.SetContent("HouseBeds",$lp($with.GetInts("Beds")));
      this.SetContent("StatSize",$lp($with.GetInts("StatSize")));
      this.SetContent("StatFurnish",$lp($with.GetInts("Furnish")));
      this.SetContent("StatYard",$lp($with.GetInts("Yard")));
      this.SetContent("StatUpKeep",$lp($with.GetInts("Upkeep")));
      this.SetContent("StatLayout",$lp($with.GetInts("Layout")));
      this.HideAllLayers();
      for (i = 0; i <= 6; i++) this.ShowLayer($mod.HouseLayers[i]);
      for (i = 1; i <= 3; i++) this.ShowLayer("FloorNav" + $lp(i));
      this.AdjustCenterBox(571,"familyhometxt");
      this.GetHTMLElement("Level1Btn").onclick = rtl.createSafeCallback(this,"Floor1Click");
      this.GetHTMLElement("Level2Btn").onclick = rtl.createSafeCallback(this,"Floor2Click");
      this.GetHTMLElement("Level3Btn").onclick = rtl.createSafeCallback(this,"Floor3Click");
      return Result;
    };
    this.FamilyHomeView = function (URl, aRoute, Params) {
      var i = 0;
      var tbl = null;
      var lnk = "";
      this.FFamilyDB.SetFilter("Id=" + Params.GetValue("FAMILY"));
      if (this.FFamilyDB.GetDataSet().FEOF) {
        window.alert("Invalid!");
        return;
      };
      this.GetHTMLElement("HousePhoto").src = this.GetFamilyDir() + "house-thumb.jpg";
      var $with = this.FFamilyDB;
      this.SetContent("FamilyName","<center>" + $with.GetStrings("Name") + "<\/center>");
      document.title = "The " + $with.GetStrings("Name") + " Family Home Page";
      this.SetContent("CashBalance","$" + $lp($with.GetInts("Cash")));
      this.SetContent("Days",$lp($with.GetInts("Days")));
      this.SetContent("Members",$lp($with.GetInts("Members")));
      this.SetContent("Friends",$lp($with.GetInts("Friends")));
      tbl = this.GetHTMLElement("MembersTable");
      if (tbl.rows.length > 1) {
        do {
          tbl.deleteRow(tbl.rows.length - 1);
        } while (!(tbl.rows.length === 1));
      };
      lnk = "#\/Family\/" + Params.GetValue("FAMILY") + "\/Member\/";
      for (var $l = 1, $end = this.FFamilyDB.GetInts("Members"); $l <= $end; $l++) {
        i = $l;
        this.FSimDB.SetFilter("ID=" + $lp(this.FFamilyDB.GetInts("Member" + $lp(i))));
        this.SetContent("MemberName" + $lp(i),"<center>" + this.FSimDB.GetStrings("Name") + "<\/center>");
        this.GetHTMLElement("MemberFace" + $lp(i)).src = this.GetFamilyDir() + "family" + $lp(i) + "_face.jpg";
        this.GetHTMLElement("Member" + $lp(i) + "Link1").href = lnk + $lp(i);
        this.GetHTMLElement("Member" + $lp(i) + "Link2").href = lnk + $lp(i);
        this.PadMembersTable();
      };
      this.FPage = 1;
      this.ShowFamilyHome(null);
      this.FPage = 1;
      this.GetHTMLElement("FamilyLink").onclick = rtl.createSafeCallback(this,"ShowFamilyHome");
      this.GetHTMLElement("FamilyLink").href = "#\/Family\/" + Params.GetValue("FAMILY");
      this.GetHTMLElement("LotLink").onclick = rtl.createSafeCallback(this,"ShowFamilyLot");
    };
    this.FamilyMemberView = function (URl, aRoute, Params) {
      var i = 0;
      this.FFamilyDB.SetFilter("ID=" + Params.GetValue("FAMILY"));
      if (this.FFamilyDB.GetDataSet().FEOF) {
        window.alert("Invalid!");
        return;
      };
      this.FSimDB.SetFilter("ID=" + $lp(this.FFamilyDB.GetInts("Member" + Params.GetValue("ID"))));
      if (this.FSimDB.GetDataSet().FEOF) {
        window.alert("Invalid!");
        return;
      };
      var $with = this.FSimDB;
      this.SetContent("SimName","<center>" + $with.GetStrings("Name") + "<\/center>");
      document.title = $with.GetStrings("Name") + "'s Biography";
      this.SetContent("SimBio",$with.GetStrings("Bio"));
      this.SetContent("SimGender",$with.GetStrings("Gender"));
      this.SetContent("SimAge",this.GetLifeStage($with.GetInts("Stage")));
      this.SetContent("SimSign",this.GetSign($with.GetInts("Sign")));
      this.SetContent("SimCareer",$with.GetStrings("Career"));
      this.SetContent("SimJob",$with.GetStrings("Job"));
      this.SetContent("SimJobPerform",$lp($with.GetInts("Performance")));
      this.SetContent("SimSalary","$" + $lp($with.GetInts("Salary")));
      this.SetContent("SimNeat",$lp($with.GetInts("Neat")));
      this.SetContent("SimOutgoing",$lp($with.GetInts("Outgoing")));
      this.SetContent("SimActive",$lp($with.GetInts("Active")));
      this.SetContent("SimPlayful",$lp($with.GetInts("Playful")));
      this.SetContent("SimNice",$lp($with.GetInts("Nice")));
      this.SetContent("SimCooking",$lp($with.GetInts("Cooking")));
      this.SetContent("SimMechanical",$lp($with.GetInts("Mechanical")));
      this.SetContent("SimCharisma",$lp($with.GetInts("Charisma")));
      this.SetContent("SimBody",$lp($with.GetInts("Body")));
      this.SetContent("SimLogic",$lp($with.GetInts("Logic")));
      this.SetContent("SimCreativity",$lp($with.GetInts("Creativity")));
      this.HideAllLayers();
      this.GetHTMLElement("SimPhoto").src = this.GetFamilyDir() + "family" + Params.GetValue("ID") + "_full.jpg";
      for (i = 0; i <= 6; i++) this.ShowLayer($mod.MemberLayers[i]);
      this.AdjustCenterBox(365,"familymembertxt");
      this.FPage = 2;
    };
    this.AlbumView = function (URl, aRoute, Params) {
      this.HideLayer("CenterBox");
      this.HideAllLayers();
      this.ShowLayer("ScrapbookView");
    };
    this.AdjustCenterBox = function (width, imgsrc) {
      var cnt = null;
      var img = null;
      var tbl = null;
      var row = null;
      this.ShowLayer("CenterBox");
      cnt = this.GetHTMLElement("CenterBox");
      cnt.style.setProperty("width",$lp(width) + "px");
      img = this.GetHTMLElement("TitleImage");
      img.src = "s\/" + imgsrc + ".jpg";
      img.width = width - 1;
      tbl = cnt.children.item(0);
      tbl.width = $lp(width);
      tbl = cnt.children.item(1);
      tbl.width = $lp(width);
      row = tbl.rows.item(0);
      row.cells.item(1).width = $lp(width);
      tbl = cnt.children.item(2);
      tbl.width = $lp(width);
      row = tbl.rows.item(0);
      row.cells.item(0).childNodes.item(0).width = width;
    };
    this.ShowHouse = function (floor) {
      var imgsrc = "";
      imgsrc = "house-floor" + $lp(floor);
      if (floor === 1) {
        this.SetContent("FloorText",this.FLotDB.GetStrings("Name") + ": First Floor Interior")}
       else if (floor === 2) {
        this.SetContent("FloorText",this.FLotDB.GetStrings("Name") + ": Second Floor Interior")}
       else if (floor === 3) {
        this.SetContent("FloorText",this.FLotDB.GetStrings("Name") + ": Exterior");
        imgsrc = "house-exterior";
      };
      this.GetHTMLElement("HouseImage").src = this.GetFamilyDir() + imgsrc + ".jpg";
    };
    this.Floor1Click = function (aEvent) {
      var Result = false;
      this.ShowHouse(1);
      Result = false;
      return Result;
    };
    this.Floor2Click = function (aEvent) {
      var Result = false;
      this.ShowHouse(2);
      Result = false;
      return Result;
    };
    this.Floor3Click = function (aEvent) {
      var Result = false;
      this.ShowHouse(3);
      Result = false;
      return Result;
    };
    this.SeeHouse = function (aEvent) {
      var Result = false;
      this.HideLayer("HouseIconLayer");
      this.HideLayer("SeeHouseLink");
      this.ShowLayer("HouseThumb");
      this.ShowLayer("SeeNeighborhoodLink");
      return Result;
    };
    this.SeeNeighborhood = function (aEvent) {
      var Result = false;
      this.ShowLayer("HouseIconLayer");
      this.HideLayer("SeeNeighborhoodLink");
      this.ShowLayer("SeeHouseLink");
      this.HideLayer("HouseThumb");
      return Result;
    };
    this.DatabaseLoaded = function () {
      this.FSimDB = this.FSimsDB.GetTable("sims");
      this.FLotDB = this.FSimsDB.GetTable("lots");
      this.FFamilyDB = this.FSimsDB.GetTable("families");
      this.ShowLayer("NavBar");
      if ($lp1().RouteFromURL() === "") $lp1().Push("\/Family\/1");
    };
    this.GetLifeStage = function (Id) {
      var Result = "";
      var $tmp = Id;
      if ($tmp === 1) {
        Result = "Kid"}
       else if ($tmp === 2) Result = "Adult";
      return Result;
    };
    this.GetSign = function (Id) {
      var Result = "";
      var $tmp = Id;
      if ($tmp === 1) {
        Result = "Leo"}
       else if ($tmp === 2) Result = "Capracorn";
      return Result;
    };
    this.GetFamilyDir = function () {
      var Result = "";
      Result = "Families\/" + this.FFamilyDB.GetStrings("Name") + "\/";
      return Result;
    };
    this.DoRun = function () {
      this.FPage = 3;
      this.GetHTMLElement("SeeHouseLnk").onclick = rtl.createSafeCallback(this,"SeeHouse");
      this.GetHTMLElement("SeeNeighborhoodLnk").onclick = rtl.createSafeCallback(this,"SeeNeighborhood");
      $lp1().InitHistory(2,"");
      $lp1().RegisterRoute("\/Family\/:FAMILY",rtl.createCallback(this,"FamilyHomeView"),false);
      $lp1().RegisterRoute("\/Family\/:FAMILY\/Member\/:ID",rtl.createCallback(this,"FamilyMemberView"),false);
      $lp1().RegisterRoute("\/ScrapBook\/:FAMILY",rtl.createCallback(this,"AlbumView"),false);
      this.FSimsDB = $lt3.$create("Create$1",[this]);
      this.FSimsDB.FDatafile = "SimsDB";
      this.FSimsDB.FOnSuccess = rtl.createSafeCallback(this,"DatabaseLoaded");
      this.FSimsDB.SetActive(true);
    };
  });
  this.ResetLayers = ["ScrapbookView","TitleText","FamilyText","MembersBox","HouseThumb","HouseCaption","HouseCaptionShadow","SeeNeighborhoodLink","FamilyStats","LinksBar","PersonalStatsBox","SimView","BioView","StatsView","SkillsView","Description","FloorShadow","TheHouse","HouseShadow","HouseInfoView","HouseStatsView","FloorCaption"];
  this.FamilyLayers = ["FamilyText","MembersBox","HouseThumb","HouseCaption","HouseCaptionShadow","SeeNeighborhoodLink","FamilyStats","LinksBar"];
  this.MemberLayers = ["PersonalStatsBox","TitleText","SimView","BioView","StatsView","SkillsView","Description"];
  this.HouseLayers = ["FamilyText","FloorShadow","TheHouse","HouseShadow","HouseInfoView","HouseStatsView","FloorCaption"];
  this.Application = null;
  $mod.$main = function () {
    $mod.Application = $lt1.$create("Create$1",[null]);
    $mod.Application.Initialize();
    $mod.Application.Run();
  };
});
