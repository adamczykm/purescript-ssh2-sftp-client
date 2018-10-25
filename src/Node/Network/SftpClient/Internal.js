'use strict';

var Client = require('ssh2-sftp-client');

exports.unsafeCreateNewClient = function(x){
  return new Client();
};

exports.connect = function(config){
  return function(sftp){
    return function(onError, onSuccess){
      console.log('before connect');
      sftp.connect(config)
        .then(function(){ console.log('connect success');onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.list = function(path){
  return function(sftp){
    return function(onError, onSuccess){
      console.log('before listing');
      sftp.list(path)
        .then(function(listing){ console.log('listing success');onSuccess(listing);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.rmdir = function(args){
    return function(sftp){
      return function(onError, onSuccess){
        sftp.rmdir(args.path, args.recursive)
          .then(function(listing){ console.log('rmdir success'); onSuccess(listing);})
          .catch(function(err) {onError(err);});
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelError();
        };
      };
  };
};

exports.mkdir = function(args){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.mkdir(args.path, args.recursive)
        .then(function(listing){ onSuccess(listing);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.end = function(sftp){
  return function(onError, onSuccess){
    sftp.end()
      .then(onSuccess(null)
      .catch(function(err) {onError(err);}));
    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelError();
    };
  };
};

exports.delete = function(dest){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.delete(dest)
        .then(function(){ onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.rename = function(paths){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.rename(paths.from, paths.to)
        .then(function(){ onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.chmod = function(args){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.chmod(args.dest, args.mode)
        .then(function(){ onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.fastGet = function(args){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.fastGet(args.remote, args.local)
        .then(function(){ onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.fastPut = function(args){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.fastPut(args.local, args.remote)
        .then(function(){ onSuccess(null);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};
