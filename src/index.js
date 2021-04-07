import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const initTheme = window.localStorage.getItem('appTheme') || 'light'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { theme: initTheme, height: window.innerHeight, width: window.innerWidth }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

const setTheme = (val) => {
  const themeColor = val === "light" ? "#F1FAEE" : "#1D3557"
  document.getElementById("favicon").setAttribute('href', `/favicon-${val}.ico`)
  document.body.style.backgroundColor = themeColor;
  document.querySelector('meta[name=apple-mobile-web-app-status-bar-style]')
    .setAttribute('content', val == "light" ? "default" : "black-translucent")
  document.querySelector('meta[name=theme-color]')
    .setAttribute('content', themeColor)
}

setTheme(initTheme)


app.ports.toggleTheme.subscribe(newTheme => {
  setTheme(newTheme)
  window.localStorage.setItem("appTheme", newTheme)
})

setTimeout(() => {
  document.querySelector('body').setAttribute("id", 'animatedPage')
}, 100)