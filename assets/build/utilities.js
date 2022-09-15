const child_process = require('node:child_process')
const fs = require('fs-extra')

module.exports.runShellCmdSync = (command) => {
  child_process.execSync(command, (err, stdout, stderr) => {
    if (err) {
      console.error(err)
      process.exit(1)
    } else {
      if (stdout) { console.log('\n' + stdout) }
      if (stderr) { console.log('\n' + stderr) }
      return
    }
  })
}

module.exports.ensureEmptyDirsExistSync = (dirs) => {
  dirs.forEach(dir => {
    fs.emptyDirSync(dir)
  })
}
