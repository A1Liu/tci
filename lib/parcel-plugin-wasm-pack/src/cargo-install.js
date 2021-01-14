const commandExists = require('command-exists');
const logger = require('@parcel/logger');

const { proc } = require('./helpers');

const installed = {};
const isInstalled = (cmdOrBin) => installed[cmdOrBin];

function cargoInstall(cmd, bin) {
  bin || (bin = cmd);
  logger.warn(`installing ${bin} for ${cmd}`);

  if (installed[bin]) {
    logger.warn(`${cmd} already installed, skipping`);
    return;
  }

  return commandExists(cmd)
    .then(() => {
      logger.warn(`${cmd} already installed, skipping`);
      installed[cmd] = installed[bin] = true;
    })
    .catch(() =>
      proc('cargo', ['--verbose', 'install', bin])
        .then((/* stdout */) => {
          installed[cmd] = installed[bin] = true;
          logger.warn(`${cmd} installed successfully!`);
          // stdout.split('\n').forEach(line => logger.verbose(line));
        })
        .catch((/* stderr */) => {
          installed[cmd] = installed[bin] = false;
          logger.warn(`something went wrong, ${cmd} not installed`);
          // stderr.split('\n').forEach(line => logger.error(line));
        }),
    );
}

module.exports = { isInstalled, cargoInstall };
