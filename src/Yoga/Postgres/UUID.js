export const genUUIDv7Impl = () => {
  const now = Date.now()
  const bytes = new Uint8Array(16)
  crypto.getRandomValues(bytes)
  // 48-bit timestamp (ms since epoch) in bytes 0-5
  bytes[0] = (now / 2 ** 40) & 0xff
  bytes[1] = (now / 2 ** 32) & 0xff
  bytes[2] = (now / 2 ** 24) & 0xff
  bytes[3] = (now / 2 ** 16) & 0xff
  bytes[4] = (now / 2 ** 8) & 0xff
  bytes[5] = now & 0xff
  // version 7
  bytes[6] = (bytes[6] & 0x0f) | 0x70
  // variant 10xx
  bytes[8] = (bytes[8] & 0x3f) | 0x80
  const hex = Array.from(bytes, b => b.toString(16).padStart(2, '0')).join('')
  return (
    hex.slice(0, 8) + '-' +
    hex.slice(8, 12) + '-' +
    hex.slice(12, 16) + '-' +
    hex.slice(16, 20) + '-' +
    hex.slice(20, 32)
  )
}

export const genUUIDv4Impl = () => crypto.randomUUID()
