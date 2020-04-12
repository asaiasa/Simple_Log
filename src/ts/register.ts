import { MINUTES, NEXT_AGENDA } from './class';
import { commonFunction as CF } from './common';
import DM from './dataManager';
import { register_id } from './id';

const dm: DM = new DM();
var minutes: MINUTES[] = [];
var agenda_list: NEXT_AGENDA[] = [];
var agenda_pop_list: number[] = [];
var new_minutes: MINUTES;
var current_page_index: number = 0; // first page = 0

window.onload = (): void => {
    addEvent();
    data_Init();
    selecter_Init();
}

window.onunload = (): void => {
    dm.del_New();
    dm.set_Agenda(agenda_list);
    dm.set_Minutes(minutes);
}

/**
 * event listener
 */
const addEvent = (): void => {
    CF.addEvent(register_id['back_btn'], back_page);
    CF.addEvent(register_id['next_btn'], refresh_page);
    CF.addEvent(register_id['register_btn'], register_all);
    CF.addEvent_i(register_id['agenda_checkbox'], change_display, 'change');
}

/**
 * get initial data from electron store
 */
const data_Init = (): void => {
    var temp: MINUTES | null;
    if ((temp = dm.get_New()) == null) {
        // alert('initialize error!\nmove previous page.');
        console.log('initialize error!\nmove previous page.');
        location.href = '../html/minutes.html';
        return;
    } else {
        new_minutes = temp;
    }
    minutes = dm.get_Minutes();
    agenda_list = dm.get_Agenda();
}

/**
 * insesrt data to select box and arrange display
 */
const selecter_Init = (): void => {
    var i: number = 0;
    agenda_list.forEach((value, index) => {
        if (index != agenda_pop_list[i])
            CF.addSelecterboxOption(register_id['agenda_selecter'], value.get_name())
        else {
            if (i < agenda_pop_list.length - 1) ++i;
        }
    });
    change_display();
}

/**
 * if checkbox is checked, hide calendar.
 */
const change_display = (): void => {
    var a_input: HTMLElement | null = document.getElementById(register_id['agenda_input']);
    var a_seslect: HTMLElement | null = document.getElementById(register_id['agenda_selecter_div']);
    var selecter_checked: boolean = CF.getCheckboxStatus(register_id['agenda_checkbox']);
    if (a_input != null && a_seslect != null) {
        if (selecter_checked) {
            a_input.style.display = 'none';
            a_seslect.style.display = 'block';
        }
        else {
            a_input.style.display = 'block';
            a_seslect.style.display = 'none';
        }
    }
}

/**
 * back to members page or previous data page
 */
const back_page = (): void => {
    if (current_page_index == 0) {
        // first page, so back to members page
        location.href = '../html/minutes.html';
    } else {
        agenda_pop_list.pop();
        --current_page_index;
        re_set_data();
    }
}

/**
 * re-register agenda and minutes data, then back to home.
 */
const register_all = (): void => {
    if (get_entered_data(true)) {
        pop_agenda_list();
        dm.set_Agenda(agenda_list);
        dm.set_Minutes(minutes);
        location.href = '../html/index.html';
    }
}

// get data and move next register
const refresh_page = (): void => {
    if (get_entered_data(false)) {
        clear_input();
        ++current_page_index;
        if (CF.getCheckboxStatus(register_id['agenda_checkbox'])) {
            agenda_pop_list.push(CF.getSelecterboxIndex(register_id['agenda_checkbox']));
        }
    }
}

/**
 * remove agenda data which used this time
 */
const pop_agenda_list = (): void => {
    if (CF.getCheckboxStatus(register_id['agenda_checkbox'])) {  // checkbox is true = agenda is selected agenda list
        agenda_pop_list.forEach(elm => agenda_list.splice(elm, 1));
    }
}

/**
 * push to new minutes data
 * @param registered - true when data input is completed(when register button is pushed)
 * @param agenda - get from html
 * @param content - get from html
 * @param conc - get from html
 * @param reason - get from html
 * @param memo - get from html
 * @param comp - get from html
 */
const push_data = (registered: boolean, agenda: string, content: string, conc: string, reason: string, memo: string, comp: boolean | null): void => {
    // new_minutesに入力データを登録
    if (registered) {   // pushed register button
        new_minutes.add_data(agenda, content, conc, reason, memo, comp);
        minutes.push(new_minutes);
    } else {    // pushed next button
        if (current_page_index == new_minutes.get_agendas().length) {   // when store new data except when first data
            new_minutes.add_data(agenda, content, conc, reason, memo, comp);
        }
        else if (current_page_index == 0 && new_minutes.get_agendas().length == 1) {    // push first data
            new_minutes.change_data(0, agenda, content, conc, reason, memo, comp);
        }
        else {  // when change existed data
            new_minutes.change_data(current_page_index, agenda, content, conc, reason, memo, comp);
        }
    }
}

/**
 * get entered data. if data is correctly entered, return true.
 * @param registered - true when data input is completed(when register button is pushed)
 */
const get_entered_data = (registered: boolean): boolean => {

    var chkbox: boolean = CF.getCheckboxStatus(register_id['agenda_checkbox']);
    var select: string = CF.getSelecterboxValue(register_id['agenda_selecter']);
    var agenda: string = CF.getText(register_id['agenda_input']);
    var content: string = CF.getText(register_id['content_input']);
    var conclusion: string = CF.getText(register_id['conclusion_input']);

    if (entered_check(chkbox, select, agenda, content, conclusion)) {
        let reason: string = CF.getText(register_id['reason_input']);
        let memo: string = CF.getText(register_id['memo_input']);
        let comp: boolean | null = statusChanger(CF.getSelecterboxValue(register_id['status_selecter']));

        if (chkbox) agenda = select;
        push_data(registered, agenda, content, conclusion, reason, memo, comp);

        return true;
    } else {
        // alert('Enter text.');
        console.log('Enter text.');
        return false;
    }

}

/**
 * change status string to boolean or null
 * @param value - comp status value
 */
const statusChanger = (value: string): boolean | null => {
    if (value == register_id['status_true']) return true;
    else if (value == register_id['status_false']) return false;
    else return null;
}

/**
 * check if the values are entered right
 * @param chkboc - checkbox whether use selecter or text input
 * @param selecter - value from agenda selecter
 * @param textinput - value of text input
 */
const entered_check = (chkboc: boolean, selecter: string, agenda: string, content: string, conc: string): boolean => {
    if (chkboc) {
        if (selecter == '' || content == '' || conc == '') return false;
        else return true;
    } else {
        if (agenda == '' || content == '' || conc == '') return false;
        else return true;
    }
}

/**
 * clear text and init checkbox
 */
const clear_input = (): void => {
    CF.setCheckboxStatus(register_id['agenda_checkbox'], false);
    selecter_Init();
    CF.setSelecterboxIndex(register_id['agenda_selecter'], 0);
    CF.setText(register_id['agenda_input'], '');
    CF.setText(register_id['content_input'], '');
    CF.setText(register_id['conclusion_input'], '');
    CF.setText(register_id['reason_input'], '');
    CF.setText(register_id['memo_input'], '');
    CF.setSelecterboxIndex(register_id['status_selecter'], 0);
    scrollTo(0, 0);
}

/**
 * set existed data
 */
const re_set_data = (): void => {
    CF.setCheckboxStatus(register_id['agenda_checkbox'], false);
    selecter_Init();
    var i: number = 0;
    var find: boolean = false;
    agenda_list.forEach((value, index) => {
        if (index != agenda_pop_list[i])
            if (value.get_name() == new_minutes.get_agenda(current_page_index)) {
                CF.setSelecterboxIndex(register_id['agenda_selecter'], index);
                find = true;
            }
            else {
                if (i < agenda_pop_list.length - 1) ++i;
            }
    });
    if (!find) {
        CF.setSelecterboxIndex(register_id['agenda_checkbox'], 0);
        CF.setText(register_id['agenda_input'], new_minutes.get_agenda(current_page_index));
    }
    CF.setText(register_id['content_input'], new_minutes.get_content(current_page_index));
    CF.setText(register_id['conclusion_input'], new_minutes.get_conclusion(current_page_index));
    CF.setText(register_id['reason_input'], new_minutes.get_reason(current_page_index));
    CF.setText(register_id['memo_input'], new_minutes.get_memo(current_page_index));
    CF.setSelecterboxIndex(register_id['status_selecter'], 0);
    scrollTo(0, 0);
}
