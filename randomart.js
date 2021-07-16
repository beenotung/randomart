let fs = require('fs')
let crypto = require('crypto')

let stream = fs.createReadStream('/dev/stdin')

const H = 9
const W = 17

const start = { y: 5, x: 9 }

const current = { ...start }

const NorthWest = 0
const NorthEast = 1
const SouthWest = 2
const SouthEast = 3

const occupancy = []

const symbols = []
symbols[0] = ' '
symbols[1] = '.'
symbols[2] = 'o'
symbols[3] = '+'
symbols[4] = '='
symbols[5] = '*'
symbols[6] = 'B'
symbols[7] = 'O'
symbols[8] = 'X'
symbols[9] = '@'
symbols[10] = '%'
symbols[11] = '&'
symbols[12] = '#'
symbols[13] = '/'
symbols[14] = '^'

function tick(bits) {
  switch (bits) {
    case NorthWest:
      current.y += current.y < H ? 1 : 0
      current.x -= current.x > 1 ? 1 : 0
      break
    case NorthEast:
      current.y += current.y < H ? 1 : 0
      current.x += current.x < W ? 1 : 0
      break
    case SouthWest:
      current.y -= current.y > 1 ? 1 : 0
      current.x -= current.x > 1 ? 1 : 0
      break
    case SouthEast:
      current.y -= current.y > 1 ? 1 : 0
      current.x += current.x < W ? 1 : 0
      break
  }
  const idx = current.y * W + current.x
  const count = occupancy[idx] || 0
  occupancy[idx] = count + 1
}

let hash = crypto.createHash('sha256')

stream.on('data', data => hash.write(data))
stream.on('end', () => {
  let digest = hash.digest()
  for (let i = 0; i < digest.length; i++) {
    let byte = digest[i]
    let bits0 = byte & 3
    byte = byte >> 2
    let bits1 = byte & 3
    byte = byte >> 2
    let bits2 = byte & 3
    byte = byte >> 2
    let bits3 = byte & 3
    tick(bits3)
    tick(bits2)
    tick(bits1)
    tick(bits0)
  }
  const end = { ...current }
  let line = `+${'-'.repeat(W)}+`
  console.log(line)
  for (let y = H; y >= 1; y--) {
    line = '|'
    for (let x = 1; x <= W; x++) {
      let idx = y * W + x
      let count = occupancy[idx] || 0
      let char =
        x === start.x && y === start.y
          ? 'S'
          : x === end.x && y === end.y
          ? 'E'
          : symbols[count] || symbols[14]
      line += char
    }
    line += '|'
    console.log(line)
  }
  line = `+${'-'.repeat(W)}+`
  console.log(line)
})
