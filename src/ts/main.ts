import { app, BrowserWindow } from "electron";
import * as path from "path";
import DM from './dataManager';

/**************************************************************************************/
import { mydebug as mydebug } from './debug';
const DEBUG: boolean = true;    // all store data is deleted when application is closed.
const TEST_MINUTES: boolean = true;
const TEST_AGENDA: boolean = true;
/**************************************************************************************/

var mainWindow: Electron.BrowserWindow | null = null;

// correspond to default value deprecated 
app.allowRendererProcessReuse = true;

function createWindow() {
    // Create the browser window.
    mainWindow = new BrowserWindow({
        height: 600,
        width: 800,
        webPreferences: {
            nodeIntegration: true
        },
    });

    // and load the index.html of the app.
    mainWindow.loadFile(path.join(__dirname, "../src/html/index.html"));

    if (DEBUG) {
        // Open the DevTools.
        mainWindow.webContents.openDevTools();
        mainWindow.maximize();
        if (TEST_MINUTES) mydebug.Store_Sample_Minutes_Data();
        if (TEST_AGENDA) mydebug.Store_Sample_Agenda_Data();
    }

    // Emitted when the window is closed.
    mainWindow.on("closed", () => {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        postProcess();
        mainWindow = null;
    });
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on("ready", createWindow);

// Quit when all windows are closed.
app.on("window-all-closed", () => {
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    try {
        if (process.platform !== "darwin") {
            app.quit();
        }
    } catch (e) {
        // alert(e);
    }
});

app.on("activate", () => {
    // On OS X it"s common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (mainWindow === null) {
        createWindow();
    }
});

// In this file you can include the rest of your app"s specific main process
// code. You can also put them in separate files and require them here.
function postProcess() {
    const dm: DM = new DM();
    if (DEBUG) {
        dm.console_all();
        dm.del_all();
    }
    dm.del_New();
    dm.del_Temp();
    dm.del_free();
    dm.del_search();
}
