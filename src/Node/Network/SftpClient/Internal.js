'use strict';

var Client = require('ssh2-sftp-client');

exports.unsafeCreateNewClient = function(x){
  return new Client();
};

exports.connect = function(config){
  return function(sftp){
    return function(onError, onSuccess){
      sftp.connect(config)
        .then(function(){ onSuccess(null);})
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
      sftp.list(path)
        .then(function(listing){ onSuccess(listing);})
        .catch(function(err) {onError(err);});
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.rmdir = function(path){
  return function(recursive){
    return function(sftp){
      return function(onError, onSuccess){
        sftp.rmdir(path, recursive)
          .then(function(listing){ onSuccess(listing);})
          .catch(function(err) {onError(err);});
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelError();
        };
      };
    };
  };
};

exports.mkdir = function(path){
  return function(recursive){
    return function(sftp){
      return function(onError, onSuccess){
        sftp.mkdir(path, recursive)
          .then(function(listing){ onSuccess(listing);})
          .catch(function(err) {onError(err);});
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelError();
        };
      };
    };
  };
};
