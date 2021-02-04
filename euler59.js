import {promises as fs} from 'fs'
import * as R from 'ramda'

const keyLength = 3

// type Key = [[Integer, Integer, Integer]]
// type Chunk = [[Integer, Integer, Integer]]

// :: String -> Integer
const parseInt = R.unary(Number.parseInt)

// :: (Integer, Integer) -> Integer
const bxor = (x, y) => x ^ y

// :: String[0] -> Integer
const charCode = s => s.charCodeAt(0)

// :: [Integer] -> String
const charCodesToString = codes => String.fromCharCode(...codes)

// :: String -> [Integer]
const stringToCharCodes = R.map(charCode)

// Promise(RegExp)
const getWordsRe = fs.readFile('files/100-common-english-words.txt', 'utf-8')
      .then(R.split('\n'))
      .then(R.join('|'))
      .then(re => new RegExp(re, 'g'))

// Promise([Chunk])
const getEncryptedChunks = fs.readFile('files/p059_cipher.txt', 'utf-8')
      .then(R.split(','))
      .then(R.map(parseInt))
      .then(R.splitEvery(keyLength))

// Promise([Key])
const getKeys = new Promise((resolve, _reject) => {
    const range = R.range(charCode('a'), charCode('z') + 1)
    const res = []
    for (let i of range) {
        for (let j of range) {
            for (let k of range) {
                res.push([i, j, k])
            }
        }
    }
    resolve(res)
})

// :: Key -> Chunk -> Chunk
const decryptChunk = R.zipWith(bxor)

// :: (Key, [Chunk]) -> [Chunk]
const decryptChunks = (key, chunks) => R.map(decryptChunk(key), chunks)

// :: [Chunk] -> String
const joinChunks = R.compose(charCodesToString, R.flatten)

// :: RegExp -> String -> Integer
const matchCount = R.compose(R.length, R.match)

// :: ([Key], RegExp, [Chunk]) -> [[Integer, String]]
const decryptAll = (keys, re, chunks) =>
      R.map(key => {
          const decrypted = joinChunks(decryptChunks(key, chunks))
          const count = matchCount(re, decrypted)
          return [count, decrypted]
      }, keys)

Promise.all([getKeys, getWordsRe, getEncryptedChunks])
    .then(([keys, re, chunks]) => decryptAll(keys, re, chunks))
    //.then(R.tap(console.log))
    .then(R.sort(R.descend(R.prop(0))))
    .then(R.path([0, 1]))
    .then(stringToCharCodes)
    .then(R.sum)
    .then(console.log)
    .catch(console.error)
