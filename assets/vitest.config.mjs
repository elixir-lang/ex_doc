import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    environment: 'jsdom',
    include: ['test/**/*.spec.js'],
    globals: true,
    coverage: {
      provider: 'v8',
      include: ['js/**/*.js']
    }
  }
})
