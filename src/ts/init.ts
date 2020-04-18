import { commonFunction as CF } from './common';
import DM from './dataManager';

window.onload = (): void => {
    CF.addEvent('init-button', init);
}

const init = (): void => {
    const dm: DM = new DM();
    dm.del_all();
    location.href = '../html/index.html';
}