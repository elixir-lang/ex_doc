const SETTINGS_KEY = 'ex_doc:settings'

const DEFAULT_SETTINGS = {
  // Whether to show tooltips on function/module links
  tooltips: true,
  // Theme preference, null if never explicitly overridden by the user
  theme: null,
  // Livebook URL to point the badges directly to
  livebookUrl: null
}

/**
 * Stores configuration state and persists it across
 * browser sessions.
 */
class SettingsStore {
  constructor () {
    this._subscribers = []
    this._settings = DEFAULT_SETTINGS

    this._loadSettings()
  }

  /**
   * Returns the current settings.
   */
  get () {
    return this._settings
  }

  /**
   * Stores the given settings in local storage.
   *
   * The given attributes are merged into the current settings.
   */
  update (newSettings) {
    const prevSettings = this._settings
    this._settings = { ...this._settings, ...newSettings }
    this._subscribers.forEach((callback) =>
      callback(this._settings, prevSettings)
    )
    this._storeSettings()
  }

  /**
   * Runs the given function with the current settings and then
   * whenever the settings change.
   */
  getAndSubscribe (callback) {
    this._subscribers.push(callback)
    callback(this._settings)
  }

  _loadSettings () {
    try {
      const json = localStorage.getItem(SETTINGS_KEY)

      if (json) {
        const settings = JSON.parse(json)
        this._settings = { ...this._settings, ...settings }
      }

      this._loadSettingsLegacy()
    } catch (error) {
      console.error(`Failed to load settings: ${error}`)
    }
  }

  _storeSettings () {
    try {
      this._storeSettingsLegacy()
      localStorage.setItem(SETTINGS_KEY, JSON.stringify(this._settings))
    } catch (error) {
      console.error(`Failed to persist settings: ${error}`)
    }
  }

  // Every package uses a specific ExDoc, so the JS used
  // across packages varies. We now store all local settings
  // under a single key, but to ensure compatibility across
  // pages, we also load/store settings from the legacy keys

  _loadSettingsLegacy () {
    const tooltipsDisabled = localStorage.getItem('tooltipsDisabled')
    if (tooltipsDisabled !== null) {
      this._settings = { ...this._settings, tooltips: false }
    }

    const nightMode = localStorage.getItem('night-mode')
    if (nightMode === 'true') {
      this._settings = { ...this._settings, nightMode: true }
    }

    if (this._settings.nightMode === true) {
      this._settings = { ...this._settings, theme: 'dark' }
    }
  }

  _storeSettingsLegacy () {
    if (this._settings.tooltips) {
      localStorage.removeItem('tooltipsDisabled')
    } else {
      localStorage.setItem('tooltipsDisabled', 'true')
    }

    if (this._settings.nightMode !== null) {
      localStorage.setItem('night-mode', this._settings.nightMode === true ? 'true' : 'false')
    } else {
      localStorage.removeItem('night-mode')
    }

    if (this._settings.theme !== null) {
      localStorage.setItem('night-mode', this._settings.theme === 'dark' ? 'true' : 'false')
      this._settings.nightMode = this._settings.theme === 'dark'
    } else {
      delete this._settings.nightMode
      localStorage.removeItem('night-mode')
    }
  }
}

export const settingsStore = new SettingsStore()
