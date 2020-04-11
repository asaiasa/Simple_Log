import { MINUTES } from './class';

export namespace subcommon {

    /**
     * return search words as typeof { 'AND':string[] 'OR':string[] }
     * @param str entered search confition (text)
     */
    const analyze_search_words = (str: string): { 'AND': string[], 'OR': string[] } => {
        const entered_words: string[] = str.replace(/　/g, ' ').split(' ');
        var words: { 'AND': string[], 'OR': string[] } = { 'AND': [], 'OR': [] };
        entered_words.forEach(s => {
            if (s.match(/".*"/) == null) words['OR'].push(s);
            else words['AND'].push(s.replace(/"/g, ''));
        });
        return words;
    }

    /**
     * search minutes data with some conditions
     * @param array - minutes class array
     * @param conditions - this array have three elements; text conditions(:word1 word2 ...) start date conditin and end date condition(ex:20201225)
     */
    export function search_Minutes(array: MINUTES[], conditions: string[]): number[] {

        const Len: number = array.length;
        var res_text: number[] = [];
        var res_sdate: number[] = [];
        var res_edate: number[] = [];
        var i: number = 0;

        // text check
        if (conditions[0] != '') {

            const words: { 'AND': string[], 'OR': string[] } = analyze_search_words(conditions[0]);
            const and_cond_length: number = words.AND.length;
            const or_cond_length: number = words.OR.length;
            var go_next_loop: boolean = false;
            var j: number = 0, k: number = 0;

            for (i = 0; i < Len; ++i) {

                const agenda: string[] = array[i].get_agendas();
                const contents: string[] = array[i].get_contents();
                const reason: string[] = array[i].get_reasons();
                const memo: string[] = array[i].get_memos();
                var find_and_words: boolean[] = [];

                // search AND condition
                for (k = 0; k < and_cond_length; ++k)
                    find_and_words.push(false);
                for (k = 0; k < and_cond_length; ++k) {

                    // search agenda
                    if (find_and_words[k] = search_word(agenda, words.AND[k])) continue;

                    // search contents
                    if (find_and_words[k] = search_word(contents, words.AND[k])) continue;

                    // search reason
                    if (find_and_words[k] = search_word(reason, words.AND[k])) continue;

                    // search memo
                    if (find_and_words[k] = search_word(memo, words.AND[k])) continue;

                }

                // search OR condition
                go_next_loop = false;

                for (k = 0; k < or_cond_length; ++k) {

                    // search agenda
                    if (go_next_loop = search_word(agenda, words.OR[k])) break;

                    // search contents
                    if (go_next_loop = search_word(contents, words.OR[k])) break;

                    // search reason
                    if (go_next_loop = search_word(reason, words.OR[k])) break;

                    // search memo
                    if (go_next_loop = search_word(memo, words.OR[k])) break;


                }

                // only and condition
                if (or_cond_length == 0 && boolArrayCheck(find_and_words, true)) res_text.push(i);
                // only or condition
                else if (and_cond_length == 0 && go_next_loop) res_text.push(i);
                // both condition
                else if (and_cond_length != 0 && or_cond_length != 0)
                    if (boolArrayCheck(find_and_words, true) && go_next_loop)
                        res_text.push(i);

            }

        } else {
            res_text = arrayInit(Len);
        }

        // start date check
        if (conditions[1] != '') {
            for (i = 0; i < Len; ++i) {
                if (Number(conditions[1]) <= Number(array[i].get_date_s_as_num(''))) {
                    res_sdate.push(i);
                }
            }
        } else {
            res_sdate = arrayInit(Len);
        }

        // end date check
        if (conditions[2] != '') {
            for (i = 0; i < Len; ++i) {
                if (Number(array[i].get_date_s_as_num('')) <= Number(conditions[2])) {
                    res_edate.push(i);
                }
            }
        } else {
            res_edate = arrayInit(Len);
        }

        return duplicatedArray(res_text, res_sdate, res_edate);

    }

    /**
     * initialize array as ...
     * [0] = 0, [1] = 1, [2] = 2, ..., [len - 1] = len - 1
     * @param len array length
     */
    export const arrayInit = (len: number): number[] => {
        var temp: number[] = [];
        for (let i: number = 0; i < len; ++i)
            temp.push(i);
        return temp;
    }

    /**
     * return duplicated value array
     * @param arr1 
     * @param arr2 
     * @param arr3 
     */
    const duplicatedArray = (arr1: number[], arr2: number[], arr3: number[]): number[] => {
        const arr1arr2: number[] = [...arr1, ...arr2];
        const dup12: number[] = arr1arr2.filter(dup_notdupedlist);
        const dup12arr3: number[] = [...dup12, ...arr3];
        const dup123: number[] = dup12arr3.filter(dup_notdupedlist);
        return dup123;

    }

    /**
     * evaluate duplication. use all-numbers=[...arr1,...arr2,...arr3].filter(this)
     * @param x 
     * @param i 
     * @param arr 
     */
    const dup_notdupedlist = (x: number, i: number, arr: number[]): boolean => {
        return arr.indexOf(x) == i && i != arr.lastIndexOf(x);
    }

    /**
     * return true/false: target string includeds the word?
     * @param array - search target
     * @param word - search word
     */
    const search_word = (array: string[], word: string): boolean => {
        const len: number = array.length;
        if (len == 0) return false;
        for (let i: number = 0; i < len; ++i)
            if (-1 < array[i].indexOf(word))
                return true;
        return false;
    }

    /**
     * check all values are equal to true or false
     * @param array - boolean array
     * @param judge - expected array contents; true or false
     */
    const boolArrayCheck = (array: boolean[], judge: boolean): boolean => {
        const len: number = array.length;
        if (len == 0) return false;
        for (let i: number = 0; i < len; ++i)
            if (array[i] != judge) return false;
        return true;
    }

}
