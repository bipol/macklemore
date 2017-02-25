import $ from "jquery";
import slick from "kenwheeler/slick";

class Carousel {

    constructor() {
        Carousel.settings = {
            slidesToShow: 1,
            infinite: false,
            arrows: false,
            adaptiveHeight: true,
            speed: 100,
        };

        this.activitySelectedCount = 0;
        this.el = $('.carousel');


        $(document).ready(() => {
            $('.carousel').slick(Carousel.settings);
        });
    }

    setupEvents() {

        $('activity__btn').on('click', (e) => {
            e.preventDefault();
            let btn = $(e.target());

            this.el.trigger('activity.select', btn.data('kw'));


            if (btn.hasClass('selected')) {
                btn.removeClass('selected');
                this.activitySelectedCount--;

            } else {
                btn.addClass('selected');
                this.activitySelectedCount++;
            }

            switch(this.activitySelectedCount) {

                case 0:
                    this.el.unslick();
                    break;
                case 1:
                    this.el.slick(Carousel.settings);
                    break;

            }


        });


    }

    updateProgress() {

    }


}

export default new Carousel();
