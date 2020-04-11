import { MINUTES } from './class';
import DM from './dataManager';
import { commonFunction as CF } from './common';
import { minutes_id } from './id';

const dm: DM = new DM();

window.onload = (): void => {
    CF.addEvent(minutes_id['next-btn'], move_next_page);
    dm.del_New();
}

/**
 * if members are entered, store them and move next page.
 */
const move_next_page = (): void => {
    var textarea: string = CF.getText(minutes_id['member_id']);
    if (textarea == "") {
        // alert('Enter members.');
        return;
    }
    else {
        var members: string[] = textarea.split(',');
        dm.set_New(new MINUTES(members));
        location.href = '../html/register.html';
    }
}

