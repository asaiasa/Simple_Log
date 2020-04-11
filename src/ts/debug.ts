import DM from './dataManager';
import { MINUTES, NEXT_AGENDA } from './class';

export namespace mydebug {

    export const Store_Sample_Agenda_Data = (): void => {
        const sample_num: number = 10;
        const name: string[] = [];
        const date: string = 'test-date';
        var i: number = 0;
        for (i = 0; i < sample_num; ++i) name.push(i.toString());
        var sample: NEXT_AGENDA[] = []
        for (i = 0; i < sample_num; ++i) {
            sample.push(new NEXT_AGENDA(name[i], date, name[i]));

        }
        const dm: DM = new DM();
        dm.set_Agenda(sample);
    }

    export const Store_Sample_Minutes_Data = (): void => {
        const sample_num: number = 10;

        const members: string[] = ['tester1', 'tester2'];
        const id: string[] = [];
        var i: number = 0;
        for (i = 0; i < sample_num; ++i) id.push(i.toString());

        var sample: MINUTES[] = [];
        var date: { [s: string]: string };
        for (i = 0; i < sample_num; ++i) {
            date = {
                year: '2020',
                month: 'Dec',
                date: (10 + i).toString(),
            };
            sample.push(new MINUTES(members, date, id[i]));
        }

        var agenda: string[][] = [
            ['this is a test agenda name.', 'sample field.'],
            ['do not mind.', 'never give up'],
            ['not interesting'],
            ['watch up!'],
            ['boolean string', 'number, date'],
            ['story teller'],
            ['cat', 'dog', 'hamster'],
            ['drive', 'swim'],
            ['coffee', 'milk', 'tea'],
            ['one piece', 'naruto', 'bleach']
        ];
        var content: string[][] = [
            ['my name is wonderful cat.', 'super cat.'],
            ['youtube is coded by go script.', ''],
            ['discode is coded by typescript.'],
            ['no worth meating'],
            ['time-up', 're-discussion next meating'],
            ['none'],
            ['none', '', ''],
            ['cover move is perfect skill', 'that is not perfect, i think.'],
            ['none', '', 'travel free'],
            ['we are ...', '', 'super sample'],
        ];
        var conc: string[][] = [
            ['reject', 'unresolved'],
            ['reject', 'unresolved'],
            ['unresolved'],
            ['unresolved'],
            ['OK', 'NG'],
            ['OK'],
            ['OK', 'OK', 'reject'],
            ['OK', 'unresolved'],
            ['unresolved', 'unresolved', 'unresolved'],
            ['OK', 'OK', 'OK'],
        ];
        var reason: string[][] = [
            ['none', ''],
            ['', ''],
            [''],
            [''],
            ['', 'none'],
            [''],
            ['', '', ''],
            ['', ''],
            ['', '', ''],
            ['', '', ''],
        ];
        var comp: (boolean | null)[][] = [
            [true, false],
            [null, null],
            [true],
            [false],
            [false, false],
            [true],
            [null, true, false],
            [null, false],
            [true, true, null],
            [true, false, false],
        ];
        for (i = 0; i < sample_num; ++i)    sample[i].set_agendas(agenda[i]);
        for (i = 0; i < sample_num; ++i)    sample[i].set_contents(content[i]);
        for (i = 0; i < sample_num; ++i)    sample[i].set_conclusions(conc[i]);
        for (i = 0; i < sample_num; ++i)    sample[i].set_reasons(reason[i]);
        for (i = 0; i < sample_num; ++i)    sample[i].set_comps(comp[i]);
        const dm: DM = new DM();
        dm.set_Minutes(sample);

    }

}
