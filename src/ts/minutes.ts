import { MINUTES } from './class';
import DM from './dataManager';
import { commonFunction as CF } from './common';
import { minutes_id } from './id';

const dm: DM = new DM();

window.onload = (): void => {
    CF.addEvent(minutes_id['back_btn'], back_page);
    CF.addEvent(minutes_id['next-btn'], move_next_page);
    init();
}

const init = (): void => {
    var temp: MINUTES | null;
    if ((temp = dm.get_New()) != null) {
        CF.setText(minutes_id['member_id'], temp.get_members().join(', '));
        var obj: HTMLLabelElement | undefined = document.getElementById(minutes_id['member_id'])?.parentElement?.getElementsByTagName('label')[0];
        if (obj !== undefined) obj.className = 'active';
    }
    dm.del_New();
}

/**
 * back to index page
 */
const back_page = (): void => {
    dm.del_Temp();
    dm.del_free();
}

/**
 * if members are entered, store them and move next page.
 */
const move_next_page = (): void => {
    var textarea: string = CF.getText(minutes_id['member_id']);
    if (textarea != "") {
        dm.set_New(new MINUTES(textarea.replace(/, /g, ',').split(',')));
        location.href = '../html/register.html';
    }
}

